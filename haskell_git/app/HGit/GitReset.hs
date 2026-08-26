{-# LANGUAGE RecordWildCards #-}

module HGit.GitReset (gitReset, ResetOptions (..), ResetMode (..)) where

import HGit.FindObject (findAndCoerceToTree, findObject, resolveRef)
import HGit.Index (readIndex)
import HGit.Object (ObjType (CommitObj), Object (..), readObj, readObjOfType)
import HGit.Repository (WithRepository, WorkTreePath, gitPath, runWithFoundRepo, worktreePath, worktreePath')
import HGit.Tree (flattenTree)
import HGit.UnpackTree (unpackTree)
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

data ResetMode = ResetSoft | ResetMixed | ResetHard deriving (Eq, Show)
data ResetOptions = ResetOptions {optMode :: ResetMode, optRef :: Text}

gitReset :: ResetOptions -> IO ()
gitReset ResetOptions{..} = runWithFoundRepo $ do
  case optMode of
    ResetSoft -> gitResetSoft optRef
    ResetMixed -> throwErr "gitReset" "umimplemented"
    ResetHard -> gitResetHard optRef

-- Leave your working tree files and the index unchanged.
gitResetSoft :: Text -> WithRepository ()
gitResetSoft ref = do
  let branchRef = "refs/heads/" <> ref
  branchExists <- Dir.doesFileExist =<< gitPath [toString branchRef]
  newHead <-
    if branchExists
      then return $ "ref: " <> branchRef
      else show . objHash <$> (readObjOfType CommitObj =<< findObject ref)

  headPath <- gitPath ["HEAD"]
  writeFileText headPath newHead

-- Overwrite all files and directories with the version from <commit>, and may
-- overwrite untracked files. Tracked files not in <commit> are removed so that
-- the working tree matches <commit>. Update the index to match  the  new  HEAD,
-- so nothing will be staged.
gitResetHard :: Text -> WithRepository ()
gitResetHard commit = do
  tree <- findAndCoerceToTree commit
  flattened <- flattenTree tree

  idx <- readIndex

  unpackTree idx flattened
  headFile <- resolveRef =<< gitPath ["HEAD"]
  newHead <- show . objHash <$> (readObjOfType CommitObj =<< findObject commit)

  writeFileText headFile newHead
  putTextLn $ "HEAD is now at " <> newHead
