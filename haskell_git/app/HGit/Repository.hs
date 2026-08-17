{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HGit.Repository (
  gitPath,
  worktreePath,
  objectsPath,
  runWithRepo,
  runWithFoundRepo,
  Repository (..),
  WithRepository (..),
) where

import HGit.Utils (throwErr)
import Relude
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath
import UnliftIO (MonadUnliftIO)

data Repository = Repository
  { repoWorktree :: FilePath
  , repoGitdir :: FilePath -- Path to .git directory
  }
  deriving (Show, Eq)

newtype WithRepository a = WithRepository
  {unWithRepository :: ReaderT Repository IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Repository, MonadUnliftIO)

runWithFoundRepo :: WithRepository a -> IO a
runWithFoundRepo action = getRepo >>= runReaderT (unWithRepository action)

runWithRepo :: Repository -> WithRepository a -> IO a
runWithRepo repo action = runReaderT (unWithRepository action) repo

-- | Compute path to a file inside .git folder (e.g., repoFile repo ["objects", "4b"])
gitPath :: [FilePath] -> WithRepository FilePath
gitPath path = do
  gitdir <- asks repoGitdir
  return $ foldl' (</>) gitdir path

worktreePath :: [FilePath] -> WithRepository FilePath
worktreePath path = do
  worktree <- asks repoWorktree
  return $ foldl' (</>) worktree path

objectsPath :: [FilePath] -> WithRepository FilePath
objectsPath path = gitPath ("objects" : path)

-- | Recursively find repo
findRepo :: IO (Maybe Repository)
findRepo = rec "."
 where
  rec :: FilePath -> IO (Maybe Repository)
  rec from =
    do
      here <- canonicalizePath from
      runMaybeT $
        MaybeT (openRepo here) <|> do
          let parent = takeDirectory here
          guard (parent /= here)
          MaybeT (rec parent)

-- | Recursively find repo, throw when no repo found
getRepo :: IO Repository
getRepo =
  findRepo >>= \case
    Nothing -> throwErr "getRepo" "Not in a git repository!"
    Just a -> return a

openRepo :: FilePath -> IO (Maybe Repository)
openRepo worktree = do
  let gitdir = worktree </> ".git"
  isDir <- doesDirectoryExist gitdir
  if isDir
    then
      return $ Just Repository{repoWorktree = worktree, repoGitdir = gitdir}
    else return Nothing
