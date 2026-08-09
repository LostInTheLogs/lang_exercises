{-# LANGUAGE RecordWildCards #-}

module HGit.Object (
  writeObj,
  makeObject,
  objHash,
  strToObjType,
  ObjType (..),
) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import GHC.Int (Int64)
import HGit.Repository
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data ObjType = Blob | Commit | Tree | Tag deriving (Show, Eq)

data Object = Object
  { objType :: !ObjType
  , objSize :: !Int64
  , objHash :: ![Char]
  , objData :: !BSL.ByteString
  }
  deriving (Show, Eq)

objTypeToStr :: ObjType -> String
objTypeToStr Blob = "blob"
objTypeToStr Commit = "commit"
objTypeToStr Tree = "tree"
objTypeToStr Tag = "tag"

strToObjType :: String -> Maybe ObjType
strToObjType "blob" = Just Blob
strToObjType "commit" = Just Commit
strToObjType "tree" = Just Tree
strToObjType "tag" = Just Tag
strToObjType _ = Nothing

objectsPath :: Repository -> [FilePath] -> FilePath
objectsPath repo path = repoPath repo ("objects" : path)

makeObject :: BSL.LazyByteString -> ObjType -> Object
makeObject contents objType =
  let objSize = BSL.length contents
      objData = addHeader objSize
      objHash = BSC8.unpack $ Base16.encode $ SHA1.hashlazy objData
   in Object{..}
 where
  addHeader len =
    let header =
          B.string8 (objTypeToStr objType)
            <> B.char8 ' '
            <> B.int64Dec len
            <> B.word8 0
        blob = header <> B.lazyByteString contents
     in B.toLazyByteString blob

writeObj :: Repository -> Object -> IO ()
writeObj repo Object{..} = do
  createDirectoryIfMissing False folderPath
  BSL.writeFile (folderPath </> fileName) compressed
 where
  compressed = Zlib.compress objData
  (folderName, fileName) = splitAt 2 objHash
  folderPath = objectsPath repo [folderName]

readObj :: Repository -> ObjType -> [Char] -> IO Object
readObj repo objHash = do
  objData <- BSL.readFile $ objectsPath repo [folderName, fileName]
  let objSize = BSL.length objData
  return Object{..}
 where
  (folderName, fileName) = splitAt 2 objHash
