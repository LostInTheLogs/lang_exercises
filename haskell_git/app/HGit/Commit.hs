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
import HGit.Object (Hash, ObjType (CommitObj), Object (..), asciiHashParser, readObj)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadLine, runParserUnsafe, throwErr)

-- import Data.Int (Int64)
-- import HGit.Repository (Repository, repoPath)
-- import HGit.Utils (note)
-- import System.Directory (createDirectoryIfMissing, emptyPermissions, setOwnerReadable, setOwnerWritable, setPermissions)
-- import System.FilePath ((</>))

data Commit = Commit
  { commitHash :: !Hash -- 20 byte
  , commitTree :: !Hash -- 20 byte
  , commitParents :: ![Hash] -- 20 byte
  , commitAuthor :: !BS.ByteString
  , commitCommitter :: !BS.ByteString
  , commitHeaderRest :: !BS.ByteString
  , commitMsg :: !BS.ByteString
  }
  deriving (Show, Eq)

commitParser :: Hash -> A.Parser Commit
commitParser commitHash = do
  commitTree <- lineParser "tree" asciiHashParser

  commitParents <- A.many' $ lineParser "parent" asciiHashParser

  commitAuthor <- lineParser "author" $ A8.takeTill (== '\n')
  commitCommitter <- lineParser "committer" $ A8.takeTill (== '\n')

  commitHeaderRest <- restHeaderParser
  commitMsg <- A.takeByteString

  return Commit{..}
 where
  lineParser name parser = (A.string name *> A8.char8 ' ' *> parser <* A8.char8 '\n') <?> show name

  restHeaderParser = restHeaderParserRec mempty <?> "restHeader"
  restHeaderParserRec acc = do
    isNL <- A.option False (True <$ A8.char8 '\n')
    if isNL
      then pure acc
      else do
        chunk <- A8.takeTill (== '\n')
        nl <- A.take 1
        restHeaderParserRec (acc <> chunk <> nl)

parseCommit :: Hash -> BSLC8.ByteString -> Commit
parseCommit hash payload = do
  runParserUnsafe (commitParser hash) payload

readCommit :: Repository -> Hash -> IO Commit
readCommit repo hash = do
  Object{..} <- readObj repo CommitObj hash
  return $ parseCommit hash objPayload
