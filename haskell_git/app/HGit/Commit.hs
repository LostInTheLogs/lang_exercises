{-# LANGUAGE RecordWildCards #-}

module HGit.Commit (
  parseCommit,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.List (stripPrefix)
import GHC.List (uncons)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadLine)

-- import Data.Int (Int64)
-- import HGit.Repository (Repository, repoPath)
-- import HGit.Utils (note)
-- import System.Directory (createDirectoryIfMissing, emptyPermissions, setOwnerReadable, setOwnerWritable, setPermissions)
-- import System.FilePath ((</>))

data Commit = Commit
  { commitTree :: !BS.ByteString
  , commitParents :: ![BS.ByteString]
  , commitRaw :: !BSL.ByteString -- Raw commit payload for reading author/message later
  }
  deriving (Show, Eq)

parseCommit payload = do
  let (headerLines, bodyLines) = break BSLC8.null (BSLC8.lines payload)
  headerLines
