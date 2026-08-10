{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Object (
  findObject,
  writeObj,
  readObj,
  makeObject,
  deserializeObjType,
  Object (..),
  ObjType (..),
) where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad (guard, unless, when)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bifunctor (first)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.Int (Int64)
import Data.List (stripPrefix)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadLine, note)
import System.Directory (createDirectoryIfMissing, emptyPermissions, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))

getHeadHash :: Repository -> IO String
getHeadHash repo = do
  refOrHead <- fReadLine $ repoPath repo ["HEAD"]
  case stripPrefix "ref: " refOrHead of
    Nothing -> return refOrHead
    Just ref -> fReadLine $ repoPath repo [ref]

-- | get hash from e.g. HEAD
findObject :: Repository -> String -> IO String
findObject repo "HEAD" = getHeadHash repo
findObject _ obj = return obj

data ObjType = Blob | Commit | Tree | Tag deriving (Eq)

instance Show ObjType where
  show :: ObjType -> String
  show = serializeObjType

data Object = Object
  { objType :: !ObjType
  , objSize :: !Int64
  , objHash :: !String
  , objPayload :: !BSL.ByteString
  , objRaw :: !BSL.ByteString
  }
  deriving (Show, Eq)

serializeObjType :: ObjType -> String
serializeObjType Blob = "blob"
serializeObjType Commit = "commit"
serializeObjType Tree = "tree"
serializeObjType Tag = "tag"

deserializeObjType :: String -> Maybe ObjType
deserializeObjType "blob" = Just Blob
deserializeObjType "commit" = Just Commit
deserializeObjType "tree" = Just Tree
deserializeObjType "tag" = Just Tag
deserializeObjType _ = Nothing

objectsPath :: Repository -> [FilePath] -> FilePath
objectsPath repo path = repoPath repo ("objects" : path)

hashRaw :: BSLC8.ByteString -> String
hashRaw = BSC8.unpack . Base16.encode . SHA1.hashlazy

makeObject :: BSL.LazyByteString -> ObjType -> Object
makeObject objPayload objType =
  let objSize = BSL.length objPayload
      objRaw = addHeader objSize
      objHash = hashRaw objRaw
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
  (folderName, fileName) = splitAt 2 objHash
  folderPath = objectsPath repo [folderName]

parseObject :: ObjType -> String -> BSL.ByteString -> Either String Object
parseObject objType objHash compressedObj = do
  let objRaw = Zlib.decompress compressedObj
      (header, nullAndPayload) = BSLC8.break (== '\0') objRaw

  (_, objPayload) <- note "Missing null byte after header" $ BSLC8.uncons nullAndPayload

  let (typeString, spaceAndSize) = BSLC8.break (== ' ') header

  (_, sizeStr) <- note "Missing space after object type" $ BSLC8.uncons spaceAndSize

  let foundType = BSLC8.unpack typeString
  when (foundType /= show objType) $ Left "Object type doesn't match expected type"

  (objSize, emptyBS) <- note "Invalid object size" $ BSLC8.readInt64 sizeStr

  unless (BSL.null emptyBS) $ Left "Trailing data after object size"
  when (objSize /= BSL.length objPayload) $ Left "Object size does not match payload length"
  when (objHash /= hashRaw objRaw) $ Left "Object hash does not match"

  pure Object{..}

readObj :: Repository -> ObjType -> String -> IO Object
readObj repo expectedType objHash = do
  let (folderName, fileName) = splitAt 2 objHash
  objRaw <- BSL.readFile $ objectsPath repo [folderName, fileName]
  case parseObject expectedType objHash objRaw of
    Left err -> ioError $ userError err
    Right obj -> return obj
