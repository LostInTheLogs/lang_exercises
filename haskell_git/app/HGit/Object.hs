{-# LANGUAGE FlexibleContexts #-}

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
import qualified Data.Vector.Strict as V
import HGit.ObjectType
import HGit.Packfile
import HGit.Repository (Repository, WithRepository, gitPath, objectsPath)
import HGit.Utils (binarySearch, fReadBSLine, fReadStrLine, nameParser, note, runParserUnsafe, runParserUnsafe2, throwErr, throwStrErr)
import Relude
import qualified Relude.File as File
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified Text.Show
import qualified UnliftIO.Directory as Dir
import qualified UnliftIO.IO as IO

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
  loose <- readLooseObj objHash
  pack <- readPackObj objHash readObj
  let found = loose <|> pack
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
  loosePath <- lift $ objectsPath [folderName, fileName]
  looseFileExists <- Dir.doesFileExist loosePath
  guard looseFileExists

  objRaw <- readFileLBS loosePath
  let decomp = Zlib.decompress objRaw
  let parser = objectFileParser objHash decomp
  pure $ runParserUnsafe parser decomp
