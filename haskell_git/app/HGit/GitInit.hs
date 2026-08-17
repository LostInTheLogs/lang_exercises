{-# LANGUAGE RecordWildCards #-}

module HGit.GitInit (InitOptions (..), gitInit) where

import Control.Exception (throwIO)
import Control.Monad
import HGit.Repository (Repository (..), gitPath, runWithRepo)
import HGit.Utils
import Relude
import System.Exit
import System.FilePath ((</>))
import System.IO.Error (alreadyExistsErrorType, mkIOError)
import qualified UnliftIO.Directory as Dir

data InitOptions = InitOptions {optPath :: FilePath}

gitInit :: InitOptions -> IO ()
gitInit InitOptions{..} = do
  let worktree = optPath
      gitdir = worktree </> ".git"

  Dir.createDirectoryIfMissing True gitdir

  contents <- Dir.listDirectory gitdir
  unless (null contents) (throwIO $ mkIOError alreadyExistsErrorType "gitInit" Nothing (Just gitdir))

  let repo = Repository{repoWorktree = worktree, repoGitdir = gitdir}
  runWithRepo repo $ do
    -- Create directory structure
    gitPath ["objects"] >>= Dir.createDirectory
    gitPath ["refs"] >>= Dir.createDirectory
    gitPath ["refs", "tags"] >>= Dir.createDirectory
    gitPath ["refs", "heads"] >>= Dir.createDirectory

    -- Write initial repository files
    descPath <- gitPath ["description"]
    writeFile descPath "Unnamed repository; edit this file 'description' to name the repository.\n"

    headPath <- gitPath ["HEAD"]
    writeFile headPath "ref: refs/heads/master\n"

    configPath <- gitPath ["config"]
    let config =
          unlines
            [ "[core]"
            , "\trepositoryformatversion = 0"
            , "\tfilemode = false"
            , "\tbare = false"
            ]
    writeFileText configPath config
