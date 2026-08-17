{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.GitLsTree (gitLsTree, LsTreeOptions (..)) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import HGit.Object (ObjType (CommitObj, TreeObj), findObject, objPayload, readObj, strToHash)
import HGit.ObjectCoerce (findAndCoerceObj)
import HGit.Repository (Repository, runWithFoundRepo)
import HGit.Tree (FileMode (..), Tree (..), TreeItem (..), flattenTree, modeToStr, objToTree, readTree)
import HGit.Utils (throwErr)
import Relude
import System.FilePath (pathSeparator, (</>))

data LsTreeOptions = LsTreeOptions {optRef :: Text, optRecurse :: Bool}

gitLsTree :: LsTreeOptions -> IO ()
gitLsTree LsTreeOptions{..} = runWithFoundRepo $ do
  tree <- objToTree <$> findAndCoerceObj TreeObj optRef
  flattened <-
    if optRecurse
      then flattenTree tree
      else return $ (\x -> (tiName x, x)) <$> treeItems tree
  mapM_ printTreeItem flattened

printTreeItem :: (MonadIO m) => (String, TreeItem) -> m ()
printTreeItem (path, TreeItem{..}) = liftIO $ do
  let typeStr = case tiMode of
        RegularFile -> "blob"
        ExecutableFile -> "blob"
        Symlink -> "blob"
        Directory -> "tree"
        Gitlink -> "commit"

  putStr $ modeToStr tiMode
  putStr " "
  putStr typeStr
  putStr " "
  putStr $ show tiHash
  putStr "    "
  putStrLn path
