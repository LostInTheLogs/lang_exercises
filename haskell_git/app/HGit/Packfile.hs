module HGit.Packfile (readPackObj) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Attoparsec.Binary as AB
import Data.Attoparsec.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Strict as V
import HGit.ObjectType
import HGit.Repository (WithRepository, objectsPath)
import HGit.Utils
import Relude
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified UnliftIO as IO
import qualified UnliftIO.Directory as Dir

readPackObj :: Hash -> (Hash -> WithRepository Object) -> WithRepository (Maybe Object)
readPackObj objHash readObj = runMaybeT $ do
  packpath <- lift $ objectsPath ["pack"]
  packExists <- Dir.doesDirectoryExist packpath
  guard packExists
  entries <- Dir.listDirectory packpath
  let packIndexes = [packpath </> f | f <- entries, Path.takeExtension f == ".idx", "pack-" `isPrefixOf` f]
  let actions = MaybeT . findObjInPack objHash readObj <$> packIndexes
  asum actions

findObjInPack :: Hash -> (Hash -> WithRepository Object) -> FilePath -> WithRepository (Maybe Object)
findObjInPack objHash readObj idxPath = runMaybeT $ do
  raw <- readFileLBS idxPath
  let PackIndex{..} = runParserUnsafe packIdxV2Parser raw

  offsetIdx <- hoistMaybe $ binarySearch idxObjectHashes objHash
  let rawOffset = idxOffsets V.! offsetIdx
  let isOffsetBig = Bits.testBit rawOffset 31
  let offset :: Word64
      offset =
        if isOffsetBig
          then idxBigOffsets V.! fromIntegral (Bits.clearBit rawOffset 31)
          else fromIntegral rawOffset

  let packFile = Path.replaceExtension idxPath ".pack"
  lift $ IO.withBinaryFile packFile IO.ReadMode $ \h -> do
    readPackObjAtOffset h (fromIntegral offset) readObj

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
readPackObjAtOffset :: IO.Handle -> Integer -> (Hash -> WithRepository Object) -> WithRepository Object
readPackObjAtOffset h offset readObj = do
  IO.hSeek h IO.AbsoluteSeek offset
  contents <- liftIO $ BSL.hGetContents h

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
  getBase :: PackObjType -> BSL.ByteString -> (WithRepository Object, BSL.ByteString)
  getBase POOfsDelta raw = do
    let (offsetDelta, rest) = runParserUnsafe2 offsetParser raw
    let base = readPackObjAtOffset h (offset - offsetDelta) readObj
    (base, rest)
  getBase PORefDelta raw = do
    let (hash, rest) = first (Hash . BSL.toStrict) $ BSL.splitAt 20 raw
    let base = readObj hash
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
  --

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
