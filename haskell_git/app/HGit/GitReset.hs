{-# LANGUAGE RecordWildCards #-}

module HGit.GitReset (gitReset, ResetOptions (..), ResetMode (..)) where

import HGit.FindObject (findObject)
import HGit.Repository (WithRepository, gitPath, runWithFoundRepo)
import HGit.Utils (throwErr)
import Relude
import qualified UnliftIO.Directory as Dir

data ResetMode = ResetSoft | ResetMixed | ResetHard deriving (Eq, Show)
data ResetOptions = ResetOptions {optMode :: ResetMode, optRef :: Text}

gitReset :: ResetOptions -> IO ()
gitReset ResetOptions{..} = runWithFoundRepo $ do
  case optMode of
    ResetSoft -> gitResetSoft optRef
    ResetMixed -> throwErr "gitReset" "umimplemented"
    ResetHard -> throwErr "gitReset" "umimplemented"

gitResetSoft :: Text -> WithRepository ()
gitResetSoft branch = do
  let branchRef = "refs/heads/" <> branch
  branchExists <- Dir.doesFileExist =<< gitPath [toString branchRef]
  newHead <-
    if branchExists
      then return $ "ref: " <> branchRef
      else show <$> findObject branch

  headPath <- gitPath ["HEAD"]
  writeFileText headPath newHead

-- delete all files in index, if already deleted ignore, delete directories if empty and were tracked, somehow
-- switch out index entries
-- checkout the index
