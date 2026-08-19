{-# LANGUAGE FlexibleContexts #-}

module HGit.ObjectType (
  findObject,
  makeObject,
  deserializeObjType,
  strToHash,
  byteHashParser,
  asciiHashParser,
  getFileHash,
  hashLazy,
  readObjType,
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
import HGit.Repository (Repository, WithRepository, gitPath, objectsPath)
import HGit.Utils (binarySearch, fReadBSLine, fReadStrLine, nameParser, note, runParserUnsafe, runParserUnsafe2, throwErr, throwStrErr)
import Relude
import qualified Relude.File as File
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified Text.Show
import qualified UnliftIO.Directory as Dir
import qualified UnliftIO.IO as IO

getHeadHash :: WithRepository Hash
getHeadHash = do
  refOrHead <- fReadStrLine =<< asks gitPath ["HEAD"]
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return $ strToHash refOrHead
    Just ref -> do
      path <- asks gitPath [ref]
      strToHash <$> fReadStrLine path

-- | get hash from e.g. HEAD
findObject :: Text -> WithRepository Hash
findObject "HEAD" = getHeadHash
findObject obj = return $ strToHash obj

newtype Hash = Hash {hashBS :: BS.ByteString} deriving (Eq, Ord)

instance Show Hash where
  show :: Hash -> String
  show (Hash bs) = decodeUtf8 (Base16.encode bs)

hashLazy :: BSL.ByteString -> Hash
hashLazy = Hash . SHA1.hashlazy

strToHash :: (ConvertUtf8 a BS.ByteString) => a -> Hash
strToHash hashText = do
  case Base16.decode (encodeUtf8 hashText) of
    Left err -> throwStrErr "strToHash" err
    Right val -> Hash val

getFileHash :: (MonadIO m) => FilePath -> m Hash
getFileHash path = liftIO $ do
  contents <- BSL.readFile path
  let obj = makeObject contents BlobObj
  return $ objHash obj

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
  { objType :: ObjType
  , objSize :: Int64 -- payload size
  , objHash :: Hash
  , objPayload :: BSL.ByteString -- payload
  , objRaw :: BSL.ByteString -- header + payload (uncompressed)
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
