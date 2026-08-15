{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Tree (
  readTree,
  objToTree,
  modeToStr,
  flattenTree,
  Tree (..),
  TreeItem (..),
  FileMode (..),
) where

import Control.Applicative (many)
import Control.Arrow (first)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.List (stripPrefix)
import qualified Data.String
import GHC.List (uncons)
import HGit.Object (Hash, ObjType (TreeObj), Object (..), byteHashParser, readObj, readObjOfType)
import HGit.Repository (Repository, repoPath)
import HGit.Utils (fReadLine, nameParser, runParserUnsafe, throwErr)
import System.FilePath ((</>))

data FileMode
  = RegularFile -- 100644
  | ExecutableFile -- 100755
  | Symlink -- 120000
  | Directory -- 040000 / 40000
  | Gitlink -- 160000
  deriving (Show, Eq)

modeParser :: A.Parser FileMode
modeParser = do
  modeStr <- A8.takeTill (== ' ')
  case modeStr of
    "100644" -> pure RegularFile
    "100755" -> pure ExecutableFile
    "120000" -> pure Symlink
    "040000" -> pure Directory
    "40000" -> pure Directory
    "160000" -> pure Gitlink
    other -> fail $ "Unknown tree object mode: " ++ show other

modeToStr :: (Data.String.IsString a) => FileMode -> a
modeToStr mode =
  case mode of
    RegularFile -> "100644"
    ExecutableFile -> "100755"
    Symlink -> "120000"
    Directory -> "040000"
    Gitlink -> "160000"

data TreeItem = TreeItem {tiMode :: FileMode, tiName :: String, tiHash :: Hash} deriving (Show, Eq)
data Tree = Tree {treeHash :: Hash, treeItems :: [TreeItem]} deriving (Show, Eq)

-- data TreeItem = TreeItem {tiMode :: BS.ByteString, tiPath :: String, tiHash :: Hash} deriving (Show, Eq)
-- data Tree = Tree {treeHash :: Hash, treeItems :: [RawTreeItem]} deriving (Show, Eq)

treeParser :: Hash -> A.Parser Tree
treeParser treeHash = nameParser "treeParser" $ do
  treeItems <- A.manyTill lnParser A.endOfInput
  return Tree{..}
 where
  lnParser :: A.Parser TreeItem
  lnParser = do
    tiMode <- modeParser <* A8.char ' '

    toPathRaw <- A8.takeTill (== '\0') <* A8.char '\0'
    let tiName = BSC8.unpack toPathRaw
    tiHash <- byteHashParser

    return TreeItem{..}

objToTree :: Object -> Tree
objToTree Object{..} = runParserUnsafe (treeParser objHash) objPayload

readTree :: Repository -> Hash -> IO Tree
readTree repo hash = do
  Object{..} <- readObjOfType repo TreeObj hash
  return $ runParserUnsafe (treeParser hash) objPayload

flattenTree :: Repository -> Tree -> IO [(FilePath, TreeItem)]
flattenTree repo tree = concat <$> mapM go (treeItems tree)
 where
  go :: TreeItem -> IO [(FilePath, TreeItem)]
  go dir@TreeItem{tiMode = Directory, tiHash = treeHash} = do
    items <- flattenTree repo =<< readTree repo treeHash
    return $ first (tiName dir </>) <$> items
  go item = return [(tiName item, item)]
