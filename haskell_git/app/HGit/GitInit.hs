{-# LANGUAGE RecordWildCards #-}

module HGit.GitInit (InitOptions (..), gitInit) where

import Control.Exception (throwIO)
import Control.Monad
import HGit.Repository (Repository (..), repoPath)
import HGit.Utils
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.IO.Error (alreadyExistsErrorType, mkIOError)

data InitOptions = InitOptions {optPath :: FilePath}

gitInit :: InitOptions -> IO ()
gitInit InitOptions{..} = do
  let worktree = optPath
      gitdir = worktree </> ".git"

  createDirectoryIfMissing True gitdir

  contents <- listDirectory gitdir
  unless (null contents) (throwIO $ mkIOError alreadyExistsErrorType "gitInit" Nothing (Just gitdir))

  let repo = Repository{repoWorktree = worktree, repoGitdir = gitdir}
  createDirectory $ repoPath repo ["objects"]
  createDirectory $ repoPath repo ["refs"]
  createDirectory $ repoPath repo ["refs", "tags"]
  createDirectory $ repoPath repo ["refs", "heads"]

  writeFile (repoPath repo ["description"]) "Unnamed repository; edit this file 'description' to name the repository.\n"
  writeFile (repoPath repo ["HEAD"]) "ref: refs/heads/master\n"

  let config = unlines ["[core]", "repositoryformatversion = 0", "filemode = false", "bare = false"]

  writeFile (repoPath repo ["config"]) config
