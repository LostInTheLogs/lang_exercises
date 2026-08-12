{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Commit (
  parseCommit,
  readCommit,
  Commit (..),
) where

import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.List (stripPrefix)
import GHC.List (uncons)
import HGit.Object (ObjType (CommitObj), Object (..), readObj)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadLine, runParserUnsafe)

-- import Data.Int (Int64)
-- import HGit.Repository (Repository, repoPath)
-- import HGit.Utils (note)
-- import System.Directory (createDirectoryIfMissing, emptyPermissions, setOwnerReadable, setOwnerWritable, setPermissions)
-- import System.FilePath ((</>))

data Commit = Commit
  { commitHash :: !BS.ByteString -- 20 byte
  , commitTree :: !BS.ByteString -- 20 byte
  , commitParents :: ![BS.ByteString] -- 20 byte
  , commitRestRaw :: !BSL.ByteString
  }
  deriving (Show, Eq)

asciiHashParser :: A.Parser BS.ByteString
asciiHashParser = work <?> "ascii hash"
 where
  work = do
    hash <- A.take 40
    case Base16.decode hash of
      Left err -> fail err
      Right val -> return val

commitParser :: BS.ByteString -> A.Parser Commit
commitParser commitHash = do
  _ <- A.string "tree "
  commitTree <- asciiHashParser <* A8.char '\n'
  let parentLineParser = A.string "parent " *> asciiHashParser <* A8.char '\n'
  commitParents <- A.many' parentLineParser
  commitRestRaw <- A.takeLazyByteString
  return Commit{..}

parseCommit :: BSC8.ByteString -> BSLC8.ByteString -> Commit
parseCommit hash payload = do
  runParserUnsafe (commitParser hash) payload

readCommit :: Repository -> BS.ByteString -> IO Commit
readCommit repo hash = do
  Object{..} <- readObj repo CommitObj hash
  return $ parseCommit hash objPayload
