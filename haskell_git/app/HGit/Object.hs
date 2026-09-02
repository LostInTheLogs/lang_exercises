{-# LANGUAGE FlexibleContexts #-}

module HGit.Object (
  getFileHash,
  hashLazy,
  makeObject,
  writeObj,
  readObj,
  readObjOfType,
  Hash (..),
  Object (..),
  ObjType (..),
) where

import qualified Codec.Compression.Zlib as Zlib -- TODO: remove dependency
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
import qualified Data.List as List (stripPrefix)
import qualified Data.Vector as V
import HGit.Packfile
import HGit.Repository (Repository, WithRepository, gitPath, objectsPath)
import HGit.Types
import HGit.Utils (binarySearch, fReadBSLine, fReadStrLine, nameParser, runParserUnsafe, runParserUnsafe2, throwErr, throwStrErr)
import qualified HGit.ZLib as HZlib
import Relude
import qualified Relude.File as File
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified Text.Show
import qualified UnliftIO.Directory as Dir
import qualified UnliftIO.IO as IO

getFileHash :: (MonadIO m) => FilePath -> m Hash
getFileHash path = liftIO $ do
  contents <- readFileLBS path
  let obj = makeObject contents BlobObj
  return $ objHash obj

writeObj :: Object -> WithRepository ()
writeObj Object{..} = do
  folderPath <- objectsPath [folderName]
  Dir.createDirectoryIfMissing False folderPath
  let path = folderPath </> fileName

  fileExists <- Dir.doesFileExist path
  when fileExists $ Dir.removeFile path

  File.writeFileLBS path compressed

  Dir.setPermissions path $ Dir.setOwnerReadable True Dir.emptyPermissions
 where
  compressed = Zlib.compress objRaw
  (folderName, fileName) = splitAt 2 $ show objHash

objectFileParser :: Hash -> BSL.ByteString -> A.Parser Object
objectFileParser expectedHash objRaw = nameParser "objectFileParser" $ do
  typeStr <- A8.takeTill (== ' ')
  _ <- A8.char ' '
  let objType = readObjType $ BSC8.unpack typeStr

  objSize <- A8.decimal
  _ <- A.word8 0
  objPayload <- A.takeLazyByteString

  let actualSize = BSL.length objPayload
  -- TODO: don't
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

readObj :: Hash -> WithRepository Object
readObj objHash = do
  found <- runMaybeT $ do
    let loose = readLooseObj objHash
    let pack = readPackObj objHash readObj
    let readers = MaybeT <$> [loose, pack]
    asum readers
  let err = throwStrErr "readObj" $ "Object '" ++ show objHash ++ "' not found"
  maybe err return found

readObjOfType :: ObjType -> Hash -> WithRepository Object
readObjOfType expectedType objHash = do
  obj <- readObj objHash
  when (objType obj /= expectedType) $ throwErr "readObjOfType" "wrong type"
  return obj

readLooseObj :: Hash -> WithRepository (Maybe Object)
readLooseObj objHash = runMaybeT $ do
  let (folderName, fileName) = splitAt 2 $ show objHash
  -- TODO: cache the byte (2 ascii chars) of the name of the directory in a set
  -- (fanout also uses the first byte)
  -- use UnliftIO.Memoize

  loosePath <- lift $ objectsPath [folderName, fileName]
  looseFileExists <- Dir.doesFileExist loosePath
  guard looseFileExists

  objRaw <- readFileBS loosePath

  let (decomp, _) = HZlib.decompressExactTwoPass objRaw lenReader

  let parser = objectFileParser objHash (toLazy decomp)

  pure $ runParserUnsafe parser (toLazy decomp)

-- [<type> <size>\0<data of size>]
lenReader :: ByteString -> Int
lenReader bs = case (BSC8.elemIndex ' ' bs, BS.elemIndex 0 bs) of
  (Nothing, _) -> throwErr "lenReader" "incomplete prefix"
  (_, Nothing) -> throwErr "lenReader" "incomplete prefix"
  (Just iSpc, Just iNull) -> do
    let prefix = BS.break (== 0) bs
    let sizeAndRest = BS.drop 1 $ BSC8.dropWhile (/= ' ') bs
    let sizeBs = BS.drop (iSpc + 1) $ BS.take iNull bs

    unsafeReadUnsignedInt sizeBs + iNull + 1
 where
  unsafeReadUnsignedInt :: BS.ByteString -> Int
  unsafeReadUnsignedInt = BS.foldl' (\acc w -> acc * 10 + fromIntegral (w - 48)) 0
  {-# INLINE unsafeReadUnsignedInt #-}
