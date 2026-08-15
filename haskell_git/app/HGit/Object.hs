{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Object (
  findObject,
  writeObj,
  readObj,
  readObjOfType,
  makeObject,
  deserializeObjType,
  strToHash,
  byteHashParser,
  asciiHashParser,
  getFileHash,
  Hash (..),
  Object (..),
  ObjType (..),
) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Applicative (asum, many, (<|>))
import qualified Control.Arrow as Arr
import qualified Control.Exception as E
import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Attoparsec.Binary as AB
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.Bits as Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL8
import Data.Int (Int64)
import Data.List (foldl', isPrefixOf, stripPrefix)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Search as VSearch
import Data.Word (Word32, Word64, Word8)
import Debug.Trace
import HGit.Repository (Repository, repoPath)
import HGit.Utils (binarySearch, fReadBSLine, fReadLine, nameParser, note, runParserUnsafe, runParserUnsafe2, throwErr)
import Options.Applicative (optional)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified System.IO as IO

getHeadHash :: Repository -> IO Hash
getHeadHash repo = do
  refOrHead <- fReadLine $ repoPath repo ["HEAD"]
  case stripPrefix "ref: " refOrHead of
    Nothing -> return $ strToHash refOrHead
    Just ref -> do strToHash <$> fReadLine (repoPath repo [ref])

-- | get hash from e.g. HEAD
findObject :: Repository -> String -> IO Hash
findObject repo "HEAD" = getHeadHash repo
findObject _ obj = return $ strToHash obj

newtype Hash = Hash BS.ByteString deriving (Eq, Ord)

hashLazy :: BSL.ByteString -> Hash
hashLazy = Hash . SHA1.hashlazy

instance Show Hash where
  show :: Hash -> String
  show (Hash bs) = BSC8.unpack $ Base16.encode bs

strToHash :: String -> Hash
strToHash hashStr = do
  case Base16.decode $ BSC8.pack hashStr of
    Left err -> throwErr "strToHash" err
    Right val -> Hash val

byteHashParser :: A.Parser Hash
byteHashParser = Hash <$> A.take 20

asciiHashParser :: A.Parser Hash
asciiHashParser = do
  hash <- A.take 40
  case Base16.decode hash of
    Left err -> fail err
    Right val -> return $ Hash val

data ObjType = BlobObj | CommitObj | TreeObj | TagObj deriving (Eq)

instance Show ObjType where
  show :: ObjType -> String
  show = serializeObjType

data Object = Object
  { objType :: !ObjType
  , objSize :: !Int64 -- payload size
  , objHash :: !Hash
  , objPayload :: !BSL.ByteString -- payload
  , objRaw :: !BSL.ByteString -- header + payload (uncompressed)
  }
  deriving (Show, Eq)

serializeObjType :: ObjType -> String
serializeObjType BlobObj = "blob"
serializeObjType CommitObj = "commit"
serializeObjType TreeObj = "tree"
serializeObjType TagObj = "tag"

deserializeObjType :: String -> Maybe ObjType
deserializeObjType "blob" = Just BlobObj
deserializeObjType "commit" = Just CommitObj
deserializeObjType "tree" = Just TreeObj
deserializeObjType "tag" = Just TagObj
deserializeObjType _ = Nothing

readObjType :: String -> ObjType
readObjType "blob" = BlobObj
readObjType "commit" = CommitObj
readObjType "tree" = TreeObj
readObjType "tag" = TagObj
readObjType _ = throwErr "deserializeObjType" "unknown type"

objectsPath :: Repository -> [FilePath] -> FilePath
objectsPath repo path = repoPath repo ("objects" : path)

makeObject :: BSL.LazyByteString -> ObjType -> Object
makeObject objPayload objType =
  let objSize = BSL.length objPayload
      objRaw = addHeader objSize
      objHash = hashLazy objRaw
   in Object{..}
 where
  addHeader len =
    let header =
          B.string8 (serializeObjType objType)
            <> B.char8 ' '
            <> B.int64Dec len
            <> B.word8 0
        blob = header <> B.lazyByteString objPayload
     in B.toLazyByteString blob

writeObj :: Repository -> Object -> IO ()
writeObj repo Object{..} = do
  Dir.createDirectoryIfMissing False folderPath
  let path = folderPath </> fileName
  BSL.writeFile path compressed
  Dir.setPermissions path $ Dir.setOwnerReadable True $ Dir.setOwnerWritable True Dir.emptyPermissions
 where
  compressed = Zlib.compress objRaw
  (folderName, fileName) = splitAt 2 $ show objHash
  folderPath = objectsPath repo [folderName]

objectFileParser :: Hash -> BSL.ByteString -> A.Parser Object
objectFileParser expectedHash objRaw = nameParser "objectFileParser" $ do
  typeStr <- A8.takeTill (== ' ')
  _ <- A8.char ' '
  let objType = readObjType $ BSC8.unpack typeStr

  objSize <- A8.decimal
  _ <- A.word8 0
  objPayload <- A.takeLazyByteString

  let actualSize = BSL.length objPayload
  when (objSize /= actualSize) $ fail "Object size mismatch"
  when (expectedHash /= hashLazy objRaw) $ fail "Object hash does not match"

  let objHash = expectedHash
  pure Object{..}

{-
GIT OBJECT LOOKUP ORDER

Check Loose Object File
    Location: .git/objects/xx/yyyy...
    If file exists, read raw zlib payload and decompress.

Fallback: Scan Individual .idx Files
    Location: .git/objects/pack/pack-*.idx
    Iterate over un-indexed .idx files and binary search each.
    If found, retrieve Byte Offset and read matching .pack.

-}

readObj :: Repository -> Hash -> IO Object
readObj repo objHash = do
  loose <- readLooseObj repo objHash
  pack <- readPackObj repo objHash
  let found = loose <|> pack
  let err = throwErr "readObj" $ "Object '" ++ show objHash ++ "' not found"
  maybe err return found

readObjOfType :: Repository -> ObjType -> Hash -> IO Object
readObjOfType repo expectedType objHash = do
  obj <- readObj repo objHash
  when (objType obj /= expectedType) $ throwErr "readObjOfType" "wrong type"
  return obj

readLooseObj :: Repository -> Hash -> IO (Maybe Object)
readLooseObj repo objHash = runMaybeT $ do
  let (folderName, fileName) = splitAt 2 $ show objHash
  let loosePath = objectsPath repo [folderName, fileName]

  looseFileExists <- liftIO $ Dir.doesFileExist loosePath
  guard looseFileExists

  objRaw <- liftIO $ BSL.readFile loosePath
  let decomp = Zlib.decompress objRaw
  let parser = objectFileParser objHash decomp
  pure $ runParserUnsafe parser decomp

readPackObj :: Repository -> Hash -> IO (Maybe Object)
readPackObj repo objHash = runMaybeT $ do
  let packpath = objectsPath repo ["pack"]
  packExists <- liftIO $ Dir.doesDirectoryExist packpath
  guard packExists
  entries <- liftIO $ Dir.listDirectory packpath
  let packIndexes = [packpath </> f | f <- entries, Path.takeExtension f == ".idx", "pack-" `isPrefixOf` f]
  let actions = findObjInPack repo objHash <$> packIndexes
  asum $ map MaybeT actions

findObjInPack :: Repository -> Hash -> FilePath -> IO (Maybe Object)
findObjInPack repo objHash idxPath = runMaybeT $ do
  raw <- liftIO $ BSL.readFile idxPath
  let PackIndex{..} = runParserUnsafe packIdxV2Parser raw

  offsetIdx <- hoistMaybe $ binarySearch idxObjectHashes objHash
  let rawOffset = idxOffsets V.! offsetIdx
      isOffsetBig = Bits.testBit rawOffset 31
      offset :: Word64
      offset =
        if isOffsetBig
          then idxBigOffsets V.! fromIntegral (Bits.clearBit rawOffset 31)
          else fromIntegral rawOffset

  let packFile = Path.replaceExtension idxPath ".pack"
  liftIO $ IO.withBinaryFile packFile IO.ReadMode $ \h -> do
    readPackObjAtOffset repo h (fromIntegral offset)

{-
n-byte type and length (3-bit type, (n-1)*7+4-bit length)
Simple | Data

Simple:
compressed data

Delta:
OBJ_REF_DELTA> base object name if
OBJ_OFS_DELTA> a negative relative offset from the delta object's position in the pack
compressed delta data
-}
readPackObjAtOffset :: Repository -> IO.Handle -> Integer -> IO Object
readPackObjAtOffset repo h offset = do
  IO.hSeek h IO.AbsoluteSeek offset
  contents <- BSL.hGetContents h

  let ((poType, poSize), packObjData) = runParserUnsafe2 packObjHeaderParser contents

  case poTypeToObjType poType of
    -- simple
    Just objType -> do
      let uncompressed = Zlib.decompress packObjData
      let obj = makeObject uncompressed objType

      let sizeMismatch = poSize /= objSize obj
      when sizeMismatch $ throwErr "packObjParser" "Size doesn't match"

      return $! obj
    -- delta
    Nothing -> do
      let (lazyBaseObj, deltaRaw) = getBase poType packObjData
      let decompressed = Zlib.decompress deltaRaw
      when (BSL.length decompressed /= poSize) $ throwErr "readPackObjAtOffset" "Delta size doesn't match"
      let delta = runParserUnsafe deltaParser decompressed
      base <- lazyBaseObj

      when (pdBaseSize delta /= objSize base) $ throwErr "readPackObjAtOffset" "Base obj size doesn't match"

      let rawObj = applyDeltas (objPayload base) delta
      let obj = makeObject rawObj (objType base)

      when (pdObjSize delta /= objSize obj) $ throwErr "readPackObjAtOffset" "Result obj size doesn't match"

      return obj
 where
  getBase :: PackObjType -> BSL.ByteString -> (IO Object, BSL.ByteString)
  getBase POOfsDelta raw = do
    let (offsetDelta, rest) = runParserUnsafe2 offsetParser raw
    let base = readPackObjAtOffset repo h (offset - offsetDelta)
    (base, rest)
  getBase PORefDelta raw = do
    let (hash, rest) = Arr.first (Hash . BSL.toStrict) $ BSL.splitAt 20 raw
    let base = readObj repo hash
    (base, rest)
  getBase _ _ = throwErr "packObjParser" "Not a delta obj, programmer error"

data PackIndex = PackIndex
  { idxFanout :: V.Vector Word32
  , idxObjectHashes :: V.Vector Hash
  , idxOffsets :: V.Vector Word32
  , idxBigOffsets :: V.Vector Word64
  , idxChecksum :: Hash
  , idxPackChecksum :: Hash
  }
  deriving (Show)

data PackObjType = POCommit | POTree | POBlob | POTag | POReserved | POOfsDelta | PORefDelta deriving (Show, Eq)
numToPOType :: (Eq a, Num a) => a -> PackObjType
numToPOType 1 = POCommit
numToPOType 2 = POTree
numToPOType 3 = POBlob
numToPOType 4 = POTag
numToPOType 5 = POReserved
numToPOType 6 = POOfsDelta
numToPOType 7 = PORefDelta
numToPOType _ = POReserved

poTypeToObjType :: PackObjType -> Maybe ObjType
poTypeToObjType POCommit = Just CommitObj
poTypeToObjType POTree = Just TreeObj
poTypeToObjType POBlob = Just BlobObj
poTypeToObjType POTag = Just TagObj
poTypeToObjType _ = Nothing

idxV2Magic :: Word32
idxV2Magic = 0xff744f63

packIdxV2Parser :: A.Parser PackIndex
packIdxV2Parser = nameParser "packIdxV2Parser" $ do
  _ <- AB.word32be idxV2Magic <?> "magic"
  _ <- AB.word32be 2
  idxFanout <- V.replicateM 256 AB.anyWord32be <?> "fanout"
  let count = fromIntegral $ V.last idxFanout
  idxObjectHashes <- V.replicateM count byteHashParser <?> "hashes"
  _ <- A.take (4 * count) <?> "crc"
  idxOffsets <- V.replicateM count AB.anyWord32be <?> "offsets"

  let largeOffsetCount = length $ V.filter (`Bits.testBit` 31) idxOffsets
  idxBigOffsets <- V.replicateM largeOffsetCount AB.anyWord64be <?> "large offsets"

  idxPackChecksum <- byteHashParser
  idxChecksum <- byteHashParser
  _ <- A.endOfInput <?> "eof"
  return PackIndex{..}

data PackDeltaInstr = PDCopy Int64 Int64 | PBInsert BS.ByteString deriving (Show)
data PackDelta = PackDelta {pdBaseSize :: Int64, pdObjSize :: Int64, pdInstrs :: [PackDeltaInstr]} deriving (Show)

applyDeltas :: BSL.ByteString -> PackDelta -> BSL.ByteString
applyDeltas base PackDelta{..} = do
  B.toLazyByteString $ foldl' foldFun mempty pdInstrs
 where
  foldFun :: B.Builder -> PackDeltaInstr -> B.Builder
  foldFun acc instr = case instr of
    PBInsert bytes -> acc <> B.byteString bytes
    PDCopy offset len -> do
      let bytes = BSL.take len (BSL.drop offset base)
      acc <> B.lazyByteString bytes

deltaParser :: A.Parser PackDelta
deltaParser = do
  pdBaseSize <- sizeParser
  pdObjSize <- sizeParser
  pdInstrs <- many deltaInstrParser
  return PackDelta{..}

deltaInstrParser :: A.Parser PackDeltaInstr
deltaInstrParser = nameParser "deltaInstrParser" $ do
  op <- A.anyWord8

  when (op == 0) $ throwErr "deltaInstrParser" "reserved instr"

  if Bits.testBit op 7
    then do
      off0 <- readByteIf (Bits.testBit op 0)
      off1 <- readByteIf (Bits.testBit op 1)
      off2 <- readByteIf (Bits.testBit op 2)
      off3 <- readByteIf (Bits.testBit op 3)

      sz0 <- readByteIf (Bits.testBit op 4)
      sz1 <- readByteIf (Bits.testBit op 5)
      sz2 <- readByteIf (Bits.testBit op 6)

      let offset = off0 .|. (off1 `Bits.shiftL` 8) .|. (off2 `Bits.shiftL` 16) .|. (off3 `Bits.shiftL` 24)
          rawSize = sz0 .|. (sz1 `Bits.shiftL` 8) .|. (sz2 `Bits.shiftL` 16)
          size = if rawSize == 0 then 0x10000 else rawSize
      return $ PDCopy (fromIntegral offset) (fromIntegral size)
    else do
      rawData <- A.take $ fromIntegral op
      return $ PBInsert rawData
 where
  readByteIf :: Bool -> A.Parser Word32
  readByteIf True = fromIntegral <$> A.anyWord8
  readByteIf False = pure 0

offsetParser :: A.Parser Integer
offsetParser = do
  dataBS <- A.takeWhileIncluding (`Bits.testBit` 7)
  let header = BS.head dataBS
      restBS = BS.tail dataBS
      initlen = header .&. 0b01111111
      offset = BS.foldl' foldFun (fromIntegral initlen) restBS
  return $ fromIntegral offset
 where
  foldFun :: Word64 -> Word8 -> Word64
  foldFun acc a =
    let x = fromIntegral $ a .&. 0b01111111
     in ((acc + 1) `Bits.shiftL` 7) .|. x

sizeParser :: A.Parser Int64
sizeParser = do
  headerBS <- A.takeWhileIncluding (`Bits.testBit` 7)
  let header = BS.head headerBS
      restBS = BS.tail headerBS
      initlen = header .&. 0b01111111
      len = fst $ BS.foldl' foldHeader (fromIntegral initlen, 7) restBS
  return $ fromIntegral len
 where
  foldHeader :: (Word64, Int) -> Word8 -> (Word64, Int)
  foldHeader (acc, shift) a =
    let x = fromIntegral $ a .&. 0b01111111
     in (acc .|. (x `Bits.shiftL` shift), shift + 7)

packObjHeaderParser :: A.Parser (PackObjType, Int64)
packObjHeaderParser = nameParser "packObjHeaderParser" $ do
  headerBS <- A.takeWhileIncluding (`Bits.testBit` 7)
  let header = BS.head headerBS
      restBS = BS.tail headerBS
      packOType = numToPOType $ (header .&. 0b01110000) `Bits.shiftR` 4
      initlen = header .&. 0b00001111
      len = fst $ BS.foldl' foldHeader (fromIntegral initlen, 4) restBS
  return (packOType, fromIntegral len)
 where
  foldHeader :: (Word64, Int) -> Word8 -> (Word64, Int)
  foldHeader (acc, shift) a =
    let x = fromIntegral $ a .&. 0b01111111
     in (acc .|. (x `Bits.shiftL` shift), shift + 7)

getFileHash :: FilePath -> IO Hash
getFileHash path = do
  contents <- BSL.readFile path
  let obj = makeObject contents BlobObj
  return $ objHash obj
