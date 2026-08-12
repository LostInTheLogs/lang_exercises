{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Object (
  findObject,
  writeObj,
  readObj,
  makeObject,
  deserializeObjType,
  hashToStr,
  strToHash,
  Object (..),
  ObjType (..),
) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Applicative (asum, (<|>))
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
import Data.Int (Int64)
import Data.List (foldl', isPrefixOf, stripPrefix)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Search as VSearch
import Data.Word (Word32, Word64, Word8)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (binarySearch, fReadBSLine, fReadLine, note, runParserUnsafe, throwErr)
import Options.Applicative (optional)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified System.IO as IO

getHeadHash :: Repository -> IO BS.ByteString
getHeadHash repo = do
  refOrHead <- fReadLine $ repoPath repo ["HEAD"]
  case stripPrefix "ref: " refOrHead of
    Nothing -> strToHash refOrHead
    Just ref -> strToHash =<< fReadLine (repoPath repo [ref])

-- | get hash from e.g. HEAD
findObject :: Repository -> String -> IO BS.ByteString
findObject repo "HEAD" = getHeadHash repo
findObject _ obj = strToHash obj

hashToStr :: BS.ByteString -> String
hashToStr = BSC8.unpack . Base16.encode

strToHash :: String -> IO BS.ByteString
strToHash hash = do
  case Base16.decode $ BSC8.pack hash of
    Left err -> ioError $ userError err
    Right val -> return val

data ObjType = BlobObj | CommitObj | TreeObj | TagObj deriving (Eq)

instance Show ObjType where
  show :: ObjType -> String
  show = serializeObjType

data Object = Object
  { objType :: !ObjType
  , objSize :: !Int64 -- payload size
  , objHash :: !BS.ByteString -- 20 byte hash
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

objectsPath :: Repository -> [FilePath] -> FilePath
objectsPath repo path = repoPath repo ("objects" : path)

makeObject :: BSL.LazyByteString -> ObjType -> Object
makeObject objPayload objType =
  let objSize = BSL.length objPayload
      objRaw = addHeader objSize
      objHash = SHA1.hashlazy objRaw
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
  (folderName, fileName) = splitAt 2 $ hashToStr objHash
  folderPath = objectsPath repo [folderName]

objectFileParser :: ObjType -> BS.ByteString -> BSL.ByteString -> A.Parser Object
objectFileParser expectedType expectedHash objRaw = do
  _ <- A.string (BSC8.pack $ show expectedType) <?> "type"
  _ <- A8.char ' ' <?> "space"

  objSize <- A8.decimal <?> "size"
  _ <- A.word8 0 <?> "null"
  objPayload <- A.takeLazyByteString <?> "payload"

  let actualSize = BSL.length objPayload
  when (objSize /= actualSize) $ fail "Object size mismatch"
  when (expectedHash /= SHA1.hashlazy objRaw) $ fail "Object hash does not match"

  let objType = expectedType
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

readObj :: Repository -> ObjType -> BS.ByteString -> IO Object
readObj repo expectedType objHash = do
  loose <- readLooseObj repo expectedType objHash
  pack <- readPackObj repo expectedType objHash
  let found = loose <|> pack
  let err = throwErr "readObj" $ "Object '" ++ hashToStr objHash ++ "' not found"
  maybe err return found

readLooseObj :: Repository -> ObjType -> BS.ByteString -> IO (Maybe Object)
readLooseObj repo expectedType objHash = runMaybeT $ do
  let (folderName, fileName) = splitAt 2 $ hashToStr objHash
  let loosePath = objectsPath repo [folderName, fileName]

  looseFileExists <- liftIO $ Dir.doesFileExist loosePath
  guard looseFileExists

  objRaw <- liftIO $ BSL.readFile loosePath
  let decomp = Zlib.decompress objRaw
  let parser = objectFileParser expectedType objHash decomp
  pure $ runParserUnsafe parser decomp

readPackObj :: Repository -> ObjType -> BS.ByteString -> IO (Maybe Object)
readPackObj repo expectedType objHash = runMaybeT $ do
  let packpath = objectsPath repo ["pack"]
  packExists <- liftIO $ Dir.doesDirectoryExist packpath
  guard packExists
  entries <- liftIO $ Dir.listDirectory packpath
  let packIndexes = [packpath </> f | f <- entries, Path.takeExtension f == ".idx", "pack-" `isPrefixOf` f]
  let actions = findObjInPack expectedType objHash <$> packIndexes
  asum $ map MaybeT actions

findObjInPack :: ObjType -> BS.ByteString -> FilePath -> IO (Maybe Object)
findObjInPack expectedType objHash idxPath = runMaybeT $ do
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
    -- TODO: parse header too
    IO.hSeek h IO.AbsoluteSeek (fromIntegral offset)
    contents <- BSL.hGetContents h
    let parser = packObjParser expectedType objHash
    -- force eval while file open
    E.evaluate $ runParserUnsafe parser contents

data PackIndex = PackIndex
  { idxFanout :: V.Vector Word32
  , idxObjectHashes :: V.Vector BS.ByteString
  , idxOffsets :: V.Vector Word32
  , idxBigOffsets :: V.Vector Word64
  , idxChecksum :: BS.ByteString
  , idxPackChecksum :: BS.ByteString
  }
  deriving (Show)

data PackObjType = POCommit | POTree | POBlob | POTag | POReserved | POOfdDelta | PORefDelta deriving (Show, Eq)
numToPOType :: (Eq a, Num a) => a -> PackObjType
numToPOType 1 = POCommit
numToPOType 2 = POTree
numToPOType 3 = POBlob
numToPOType 4 = POTag
numToPOType 5 = POReserved
numToPOType 6 = POOfdDelta
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
packIdxV2Parser = do
  _ <- AB.word32be idxV2Magic <?> "magic"
  _ <- AB.word32be 2
  idxFanout <- V.replicateM 256 AB.anyWord32be <?> "fanout"
  let count = fromIntegral $ V.last idxFanout
  idxObjectHashes <- V.replicateM count (A.take 20) <?> "hashes"
  _ <- A.take (4 * count) <?> "crc"
  idxOffsets <- V.replicateM count AB.anyWord32be <?> "offsets"

  let largeOffsetCount = length $ V.filter (`Bits.testBit` 31) idxOffsets
  idxBigOffsets <- V.replicateM largeOffsetCount AB.anyWord64be <?> "large offsets"

  idxPackChecksum <- A.take 20
  idxChecksum <- A.take 20
  _ <- A.endOfInput <?> "eof"
  return PackIndex{..}

packObjParser :: ObjType -> BSC8.ByteString -> A.Parser Object
packObjParser expectedType expectedHash = do
  (poType, poSize) <- packObjHeaderParser

  case poTypeToObjType poType of
    Just objType -> do
      when (objType /= expectedType) $ throwErr "packObjParser" "Type doesn't match"

      compressed <- A.takeLazyByteString
      let uncompressed = Zlib.decompress compressed
      let obj = makeObject uncompressed objType

      let sizeMismatch = fromIntegral poSize /= objSize obj
      when sizeMismatch $ throwErr "packObjParser" "Size doesn't match"

      when (objHash obj /= expectedHash) $ throwErr "packObjParser" "Hash doesn't match"
      return obj
    Nothing -> throwErr "packObjParser" (show poType)

packObjHeaderParser :: A.Parser (PackObjType, Word64)
packObjHeaderParser = do
  headerBS <- A.takeWhileIncluding (`Bits.testBit` 7)
  let header = BS.head headerBS
      restBS = BS.tail headerBS
      packOType = numToPOType $ (header .&. 0b01110000) `Bits.shiftR` 4
      initlen = header .&. 0b00001111
      len = fst $ BS.foldl' foldHeader (fromIntegral initlen, 4) restBS
  return (packOType, len)
 where
  foldHeader :: (Word64, Int) -> Word8 -> (Word64, Int)
  foldHeader (acc, shift) a =
    let x = fromIntegral $ a .&. 0b01111111
     in (acc .|. (x `Bits.shiftL` shift), shift + 7)
