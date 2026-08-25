{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HGit.Tree (
  readTree,
  objToTree,
  modeToStr,
  flattenTree,
  Tree (..),
  TreeItem (..),
  FileMode (..),
  FlattenedTree,
) where

import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.String
import qualified FlatParse.Basic as FP
import HGit.Object (Hash, ObjType (TreeObj), Object (..), readObj, readObjOfType)
import HGit.Repository (Repository, WithRepository (WithRepository), WorkTreePath, gitPath)
import HGit.Types (byteHashFParser, byteHashParser)
import HGit.Utils
import Language.Haskell.TH
import Relude
import System.FilePath ((</>))

data FileMode
  = RegularFile -- 100644
  | ExecutableFile -- 100755
  | Symlink -- 120000
  | Directory -- 040000 / 40000
  | Gitlink -- 160000
  deriving (Show, Eq)

modeParser :: Parser FileMode
modeParser =
  $( FP.switch
      [|
        case _ of
          "100644" -> pure RegularFile
          "100755" -> pure ExecutableFile
          "120000" -> pure Symlink
          "040000" -> pure Directory
          "40000" -> pure Directory
          "160000" -> pure Gitlink
        |]
   )

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

treeParser :: Hash -> Parser Tree
treeParser treeHash = do
  treeItems <- FP.many lnParser
  FP.eof
  return Tree{..}
 where
  lnParser :: Parser TreeItem
  lnParser = do
    tiMode <- modeParser <* $(FP.char ' ')

    toPathRaw <- FP.anyCString
    let tiName = BSC8.unpack toPathRaw
    tiHash <- byteHashFParser

    return TreeItem{..}

objToTree :: Object -> Tree
objToTree Object{..} = runFParserUnsafe (treeParser objHash) (toStrict objPayload)

readTree :: Hash -> WithRepository Tree
readTree hash = do
  Object{..} <- readObjOfType TreeObj hash
  return $ runFParserUnsafe (treeParser hash) (toStrict objPayload)

type FlattenedTree = [(FilePath, TreeItem)]

flattenTree :: Tree -> WithRepository FlattenedTree
flattenTree tree = reverse <$> go "" (treeItems tree) []
 where
  go :: WorkTreePath -> [TreeItem] -> FlattenedTree -> WithRepository FlattenedTree
  go _ [] acc = return acc
  go path (dir@TreeItem{tiMode = Directory, tiHash = treeHash} : rest) acc = do
    newTree <- readTree treeHash
    newAcc <- go (path </> tiName dir) (treeItems newTree) acc
    go path rest newAcc
  go path (item : rest) acc = go path rest $ (path </> tiName item, item) : acc
