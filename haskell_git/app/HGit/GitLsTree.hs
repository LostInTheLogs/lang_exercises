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
import HGit.Repository (Repository, getRepo)
import HGit.Tree (FileMode (..), Tree (..), TreeItem (..), modeToStr, objToTree, readTree)
import HGit.Utils (throwErr)
import System.FilePath (pathSeparator, (</>))

data LsTreeOptions = LsTreeOptions {optRef :: String, optRecurse :: Bool}

gitLsTree :: LsTreeOptions -> IO ()
gitLsTree opts@LsTreeOptions{..} = do
  repo <- getRepo
  tree <- objToTree <$> findAndCoerceObj repo TreeObj optRef
  mapM_ (printTreeItem repo opts "") (treeItems tree)

printTreeItem :: Repository -> LsTreeOptions -> FilePath -> TreeItem -> IO ()
printTreeItem repo opts@LsTreeOptions{} path item@TreeItem{..} = do
  let prFile = printTreeFile repo opts path item
  let prDir = printTreeDir repo opts (path </> tiPath) item
  case tiMode of
    RegularFile -> prFile
    ExecutableFile -> prFile
    Symlink -> prFile
    Directory -> prFile
    Gitlink -> throwErr "printTreeItem" "'commit' not implemented"

printTreeDir :: Repository -> LsTreeOptions -> FilePath -> TreeItem -> IO ()
printTreeDir repo opts@LsTreeOptions{} path TreeItem{..} = do
  tree <- readTree repo tiHash
  mapM_ (printTreeItem repo opts path) (treeItems tree)

printTreeFile :: Repository -> LsTreeOptions -> FilePath -> TreeItem -> IO ()
printTreeFile _ LsTreeOptions{} path TreeItem{..} = do
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
  unless (null path) $ putStr $ path ++ [pathSeparator]
  putStrLn tiPath
