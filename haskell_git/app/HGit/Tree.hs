{-# LANGUAGE RecordWildCards #-}

module HGit.Tree (
  readTree,
  treeParser,
  Tree (..),
  TreeItem (..),
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
import HGit.Object (Hash, ObjType (TreeObj), Object (..), byteHashParser, readObj)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadLine, runParserUnsafe, throwErr)

data TreeItem = TreeItem {tiMode :: BS.ByteString, tiPath :: String, tiHash :: Hash} deriving (Show, Eq)
data Tree = Tree {treeHash :: Hash, treeItems :: [TreeItem]} deriving (Show, Eq)

-- data TreeItem = TreeItem {tiMode :: BS.ByteString, tiPath :: String, tiHash :: Hash} deriving (Show, Eq)
-- data Tree = Tree {treeHash :: Hash, treeItems :: [RawTreeItem]} deriving (Show, Eq)

treeParser :: Hash -> A.Parser Tree
treeParser treeHash = do
  treeItems <- A.manyTill lnParser A.endOfInput
  return Tree{..}
 where
  lnParser :: A.Parser TreeItem
  lnParser = do
    tiModeRaw <- A8.takeTill (== ' ') <* A8.char ' '
    let tiMode = if BS.length tiModeRaw == 5 then BSC8.cons '0' tiModeRaw else tiModeRaw

    toPathRaw <- A8.takeTill (== '\0') <* A8.char '\0'
    let tiPath = BSC8.unpack toPathRaw
    tiHash <- byteHashParser

    return TreeItem{..}

readTree :: Repository -> Hash -> IO Tree
readTree repo hash = do
  Object{..} <- readObj repo TreeObj hash
  return $ runParserUnsafe (treeParser hash) objPayload
