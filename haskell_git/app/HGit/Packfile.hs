module HGit.Packfile (readPackObj) where

import Control.Monad.Extra (firstJustM)
import qualified Data.Attoparsec.Binary as AB
import Data.Attoparsec.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified FlatParse.Basic as FP
import Foreign (peekElemOff)
import Foreign.Ptr (castPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import HGit.Repository (PackCache (..), WithRepository, objectsPath, repoPackCache)
import HGit.Types
import HGit.Utils
import qualified HGit.ZLib as HZlib
import Relude
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified System.IO.MMap as MMap
import System.IO.Unsafe (unsafePerformIO)
import qualified UnliftIO as IO
import qualified UnliftIO.Directory as Dir

getIndexFiles :: WithRepository [FilePath]
getIndexFiles = do
  cache <- asks repoPackCache
  let ref = pcIndexFiles cache
  packPaths <- readIORef ref
  case packPaths of
    Just paths -> return paths
    Nothing -> do
      packpath <- objectsPath ["pack"]
      packExists <- Dir.doesDirectoryExist packpath
      if packExists
        then do
          entries <- Dir.listDirectory packpath
          let packIndexes = [packpath </> f | f <- entries, Path.takeExtension f == ".idx", "pack-" `isPrefixOf` f]
          writeIORef ref $ Just packIndexes
          return packIndexes
        else do
          writeIORef ref Nothing
          return []

readPackObj :: Hash -> (Hash -> WithRepository Object) -> WithRepository (Maybe Object)
readPackObj objHash readObj = do
  indexFiles <- getIndexFiles
  firstJustM (findObjInPack objHash readObj) indexFiles

binarySearchHashStr :: ByteString -> Int -> Hash -> Maybe Int
binarySearchHashStr hashes n hash = loop 0 (n - 1)
 where
  needle = fromShort $ hashBS hash
  loop low high
    | low > high = Nothing
    | otherwise =
        let mid = low + (high - low) `div` 2
            val = at mid
         in case compare needle val of
              LT -> loop low (mid - 1)
              GT -> loop (mid + 1) high
              EQ -> Just mid
  at x = BSU.unsafeTake 20 (BSU.unsafeDrop (x * 20) hashes)

getIndex :: FilePath -> WithRepository (PackIndex, BS.ByteString)
getIndex idxPath = do
  cache <- asks repoPackCache
  let ref = pcIndexes cache
  indexes <- readIORef ref

  case Map.lookup idxPath indexes of
    Just found -> return found
    Nothing -> do
      raw <- liftIO $ MMap.mmapFileByteString idxPath Nothing
      let idx = runFParserUnsafe packIdxV2FParser raw

      let packFile = Path.replaceExtension idxPath ".pack"
      packRaw <- liftIO $ MMap.mmapFileByteString packFile Nothing
      writeIORef ref $ Map.insert idxPath (idx, packRaw) indexes
      return (idx, packRaw)

findObjInPack :: Hash -> (Hash -> WithRepository Object) -> FilePath -> WithRepository (Maybe Object)
findObjInPack objHash readObj idxPath = runMaybeT $ do
  (PackIndex{..}, contents) <- lift $ getIndex idxPath

  let count = fromIntegral $ UV.last idxFanout
  offsetIdx <- hoistMaybe $ binarySearchHashStr idxObjectHashes count objHash
  let rawOffset = idxOffsets `indexWord32BE` offsetIdx
  let isOffsetBig = Bits.testBit rawOffset 31
  let offset :: Word64
      offset =
        if isOffsetBig
          then idxBigOffsets `indexWord64BE` fromIntegral (Bits.clearBit rawOffset 31)
          else fromIntegral rawOffset

  lift $ readPackObjAtOffset contents (fromIntegral offset) readObj

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
readPackObjAtOffset :: BS.ByteString -> Int64 -> (Hash -> WithRepository Object) -> WithRepository Object
readPackObjAtOffset h offset readObj = do
  let contents = BS.drop (fromIntegral offset) h

  -- TODO: flatparse, no lazy bytestring anywhere
  let ((poType, poSize), packObjData) = runParserUnsafe2 packObjHeaderParser (fromStrict contents)

  case poTypeToObjType poType of
    -- simple
    Just objType -> do
      let decompressed = fst $ HZlib.decompressExact (toStrict packObjData) poSize
      let obj = makeObject (toLazy decompressed) objType
      return obj
    -- delta
    Nothing -> do
      let (lazyBaseObj, deltaRaw) = getBase poType packObjData
      let decompressed = fst $ HZlib.decompressExact (toStrict deltaRaw) poSize

      let delta = runFParserUnsafe deltaFParser decompressed
      base <- lazyBaseObj

      -- when (pdBaseSize delta /= objSize base) $ throwErr "readPackObjAtOffset" "Base obj size doesn't match"

      let rawObj = applyDeltas (objPayload base) delta
      let obj = makeObject rawObj (objType base)

      -- when (pdObjSize delta /= objSize obj) $ throwErr "readPackObjAtOffset" "Result obj size doesn't match"

      return obj
 where
  getBase :: PackObjType -> BSL.ByteString -> (WithRepository Object, BSL.ByteString)
  getBase POOfsDelta raw = do
    let (offsetDelta, rest) = runParserUnsafe2 offsetParser raw
    let base = readPackObjAtOffset h (offset - offsetDelta) readObj
    (base, rest)
  getBase PORefDelta raw = do
    let (hash, rest) = first (Hash . toShort . toStrict) $ BSL.splitAt 20 raw
    let base = readObj hash
    (base, rest)
  getBase _ _ = throwErr "packObjParser" "Not a delta obj, programmer error"

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

{-# NOINLINE packIdxV2FParser #-}
packIdxV2FParser :: Parser PackIndex
packIdxV2FParser = do
  magic <- FP.anyWord32be
  unless (magic == idxV2Magic) $ FP.err "magic"
  ver <- FP.anyWord32be
  unless (ver == 2) $ FP.err "ver != 2"

  fanoutBlock <- FP.take (256 * 4)
  let idxFanout = parseWord32BEVector fanoutBlock 256
  let count = fromIntegral $ UV.last idxFanout

  idxObjectHashes <- FP.take (count * 20)

  FP.skip (4 * count) -- crc
  idxOffsets <- FP.take (count * 4)

  remainingBytes <- FP.unPos <$> FP.getPos

  idxBigOffsets <- FP.take (remainingBytes - 40)

  idxPackChecksum <- byteHashFParser
  idxChecksum <- byteHashFParser

  FP.eof
  return PackIndex{..}
 where
  parseWord32BEVector :: BS.ByteString -> Int -> UV.Vector Word32
  parseWord32BEVector bs count = unsafePerformIO $
    BSU.unsafeUseAsCStringLen bs $ \(ptr, _) ->
      UV.generateM count $ \i -> do
        w <- peekElemOff (castPtr ptr) i
        pure $ case targetByteOrder of
          BigEndian -> w
          LittleEndian -> byteSwap32 w
  {-# INLINE parseWord32BEVector #-}

  parseWord64BEVector :: BS.ByteString -> Int -> UV.Vector Word64
  parseWord64BEVector bs count = unsafePerformIO $
    BSU.unsafeUseAsCStringLen bs $ \(ptr, _) ->
      UV.generateM count $ \i -> do
        w <- peekElemOff (castPtr ptr) i
        pure $ case targetByteOrder of
          BigEndian -> w
          LittleEndian -> byteSwap64 w
  {-# INLINE parseWord64BEVector #-}

indexWord32BE :: BS.ByteString -> Int -> Word32
indexWord32BE bs i = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, _) -> do
    w <- peekElemOff (castPtr ptr) i
    pure $ case targetByteOrder of
      BigEndian -> w
      LittleEndian -> byteSwap32 w
{-# INLINE indexWord32BE #-}

indexWord64BE :: BS.ByteString -> Int -> Word64
indexWord64BE bs i = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, _) -> do
    w <- peekElemOff (castPtr ptr) i
    pure $ case targetByteOrder of
      BigEndian -> w
      LittleEndian -> byteSwap64 w
{-# INLINE indexWord64BE #-}

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

deltaFParser :: Parser PackDelta
deltaFParser = do
  pdBaseSize <- fromIntegral <$> FP.anyVarintProtobuf
  pdObjSize <- fromIntegral <$> FP.anyVarintProtobuf
  pdInstrs <- many deltaInstrFParser
  return PackDelta{..}

deltaInstrFParser :: Parser PackDeltaInstr
deltaInstrFParser = do
  op <- FP.anyWord8

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
      rawData <- FP.take $ fromIntegral op
      return $ PBInsert rawData
 where
  readByteIf :: Bool -> Parser Word32
  readByteIf True = fromIntegral <$> FP.anyWord8
  readByteIf False = pure 0

offsetParser :: A.Parser Int64
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

packObjHeaderParser :: A.Parser (PackObjType, Int)
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
