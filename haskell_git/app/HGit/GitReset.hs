{-# LANGUAGE RecordWildCards #-}

module HGit.GitReset (gitReset, ResetOptions (..), ResetMode (..)) where

import HGit.FindObject (findAndCoerceToTree, findObject, resolveRef)
import HGit.GitSwitch (setHeadToBranch)
import HGit.Index (readIndex)
import HGit.Object (ObjType (CommitObj), Object (..), readObj, readObjOfType)
import HGit.Repository (WithRepository, WorkTreePath, gitPath, runWithFoundRepo, worktreePath, worktreePath')
import HGit.Tree (flattenTree)
import HGit.UnpackTree (UnpackTreeOpts (..), unpackTree)
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

data ResetMode = ResetSoft | ResetMixed | ResetHard deriving (Eq, Show)
data ResetOptions = ResetOptions {optMode :: ResetMode, optRef :: Text}

gitReset :: ResetOptions -> IO ()
gitReset ResetOptions{..} = runWithFoundRepo $ do
  case optMode of
    ResetSoft -> setHeadToBranch optRef
    ResetMixed -> throwErr "gitReset" "umimplemented"
    ResetHard -> gitResetHard optRef

-- Overwrite all files and directories with the version from <commit>, and may
-- overwrite untracked files. Tracked files not in <commit> are removed so that
-- the working tree matches <commit>. Update the index to match  the  new  HEAD,
-- so nothing will be staged.
gitResetHard :: Text -> WithRepository ()
gitResetHard commit = do
  tree <- findAndCoerceToTree commit
  flattened <- flattenTree tree

  idx <- readIndex

  unpackTree UnpackTreeOpts{utoCheckConflicts = False} idx flattened
  headFile <- resolveRef =<< gitPath ["HEAD"]
  newHead <- show . objHash <$> (readObjOfType CommitObj =<< findObject commit)

  writeFileText headFile newHead
  putTextLn $ "HEAD is now at " <> newHead
