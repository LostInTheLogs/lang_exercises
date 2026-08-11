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
import Control.Monad (guard, unless, when)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.List (stripPrefix)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadBSLine, fReadLine, note, runParserUnsafe)
import System.Directory (createDirectoryIfMissing, emptyPermissions, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))

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
  , objSize :: !Int64
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
  createDirectoryIfMissing False folderPath
  let path = folderPath </> fileName
  BSL.writeFile path compressed
  setPermissions path $ setOwnerReadable True $ setOwnerWritable True emptyPermissions
 where
  compressed = Zlib.compress objRaw
  (folderName, fileName) = splitAt 2 $ hashToStr objHash
  folderPath = objectsPath repo [folderName]

objectParser :: ObjType -> BS.ByteString -> BSL.ByteString -> A.Parser Object
objectParser expectedType expectedHash objRaw = do
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

readObj :: Repository -> ObjType -> BS.ByteString -> IO Object
readObj repo expectedType objHash = do
  let (folderName, fileName) = splitAt 2 $ hashToStr objHash
  objRaw <- BSL.readFile $ objectsPath repo [folderName, fileName]

  let decomp = Zlib.decompress objRaw
  let parser = objectParser expectedType objHash decomp
  return $ runParserUnsafe parser decomp
