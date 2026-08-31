{-# LANGUAGE FlexibleContexts #-}

module HGit.Types (
  Hash (..),
  Object (..),
  ObjType (..),
  PackIndex (..),
  asciiToHash,
  hashToAscii,
  zeroHash,
  zeroAsciiHash,
  hashLazy,
  byteHashBuilder,
  byteHashParser,
  byteHashFParser,
  asciiHashParser,
  asciiHashFParser,
  makeObject,
  readObjType,
  objTypeToStr,
  objTypeFromStr,
) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Relude
import System.FilePath ((</>))
import qualified Text.Show

import qualified Data.Attoparsec.Lazy as A
import Data.String.Conversions (ConvertibleStrings)
import Data.String.Conversions.Monomorphic (fromStrictByteString, toStrictByteString)
import qualified FlatParse.Basic as FP
import HGit.Utils (Parser, binarySearch, fReadBSLine, fReadStrLine, nameParser, runParserUnsafe, runParserUnsafe2, throwErr, throwStrErr)

newtype Hash = Hash {hashBS :: ShortByteString} deriving (Eq, Ord)

instance Hashable Hash where
  hashWithSalt salt Hash{..} =
    salt `hashWithSalt` hashBS

zeroHash :: Hash
zeroHash = Hash $ toShort $ BS.replicate 20 0

zeroAsciiHash :: [Char]
zeroAsciiHash = replicate 40 '0'

byteHashBuilder :: Hash -> B.Builder
byteHashBuilder hash = B.byteString $ fromShort $ hashBS hash

byteHashParser :: A.Parser Hash
byteHashParser = Hash . toShort <$> A.take 20

asciiHashParser :: A.Parser Hash
asciiHashParser = do
  hash <- A.take 40
  case Base16.decode hash of
    Left err -> fail err
    Right val -> return $ Hash $ toShort val

asciiToHash :: (ConvertibleStrings a BS.ByteString) => a -> Hash
asciiToHash hashText = do
  case Base16.decode (toStrictByteString hashText) of
    Left err -> throwStrErr "asciiToHash" err
    Right val -> Hash $ toShort val

byteHashFParser :: Parser Hash
byteHashFParser = Hash . toShort <$> FP.take 20

asciiHashFParser :: Parser Hash
asciiHashFParser = do
  hash <- FP.take 40
  case Base16.decode hash of
    Left err -> FP.err $ toText err
    Right val -> return $ Hash $ toShort val

instance Show Hash where
  show :: Hash -> String
  show = hashToAscii

hashToAscii :: (IsString a, ConvertibleStrings ByteString a) => Hash -> a
hashToAscii hash | zeroHash == hash = fromString zeroAsciiHash
hashToAscii (Hash bs) = fromStrictByteString (Base16.encode (fromShort bs))

hashLazy :: BSL.ByteString -> Hash
hashLazy = Hash . toShort . SHA1.hashlazy

data ObjType = BlobObj | CommitObj | TreeObj | TagObj deriving (Eq)

instance Show ObjType where
  show :: ObjType -> String
  show = objTypeToStr

objTypeToStr :: ObjType -> String
objTypeToStr BlobObj = "blob"
objTypeToStr CommitObj = "commit"
objTypeToStr TreeObj = "tree"
objTypeToStr TagObj = "tag"

objTypeFromStr :: String -> Maybe ObjType
objTypeFromStr "blob" = Just BlobObj
objTypeFromStr "commit" = Just CommitObj
objTypeFromStr "tree" = Just TreeObj
objTypeFromStr "tag" = Just TagObj
objTypeFromStr _ = Nothing

readObjType :: String -> ObjType
readObjType "blob" = BlobObj
readObjType "commit" = CommitObj
readObjType "tree" = TreeObj
readObjType "tag" = TagObj
readObjType _ = throwErr "readObjType" "unknown type"

data Object = Object
  { objType :: ObjType
  , objSize :: Int64 -- payload size
  , objHash :: Hash
  , objPayload :: BSL.ByteString -- payload
  , objRaw :: BSL.ByteString -- header + payload (uncompressed)
  }
  deriving (Show, Eq)

makeObject :: BSL.LazyByteString -> ObjType -> Object
makeObject objPayload objType =
  let objSize = BSL.length objPayload
      objRaw = addHeader objSize
      objHash = hashLazy objRaw
   in Object{..}
 where
  addHeader len =
    let header =
          B.string8 (objTypeToStr objType)
            <> B.char8 ' '
            <> B.int64Dec len
            <> B.word8 0
        blob = header <> B.lazyByteString objPayload
     in B.toLazyByteString blob

data PackIndex = PackIndex
  { idxFanout :: UV.Vector Word32
  , idxObjectHashes :: BS.ByteString
  , idxOffsets :: BS.ByteString
  , idxBigOffsets :: BS.ByteString
  , idxChecksum :: Hash
  , idxPackChecksum :: Hash
  }
  deriving (Show)
