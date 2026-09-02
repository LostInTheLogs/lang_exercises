{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HGit.Repository (
  gitPath,
  worktreePath,
  worktreePath',
  objectsPath,
  packPath,
  headsPath,
  runWithRepo,
  runWithFoundRepo,
  toWorktreePath,
  makeRepo,
  Repository (..),
  WithRepository (..),
  WorkTreePath,
  PackCache (..),
) where

import qualified Data.List as List
import qualified Data.Map as Map
import HGit.Types
import HGit.Utils (throwErr, throwStrErr)
import Relude
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Directory as Dir

data PackCache = PackCache
  { pcIndexFiles :: IORef (Maybe [FilePath])
  , pcIndexes :: IORef (Map FilePath (PackIndex, ByteString))
  }

data Repository = Repository
  { repoWorktree :: FilePath
  , repoGitdir :: FilePath -- Path to .git directory
  , repoPackCache :: PackCache
  }

newtype WithRepository a = WithRepository
  {getWithRepository :: ReaderT Repository IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Repository, MonadUnliftIO)

runWithFoundRepo :: WithRepository a -> IO a
runWithFoundRepo action = getRepo >>= runReaderT (getWithRepository action)

runWithRepo :: Repository -> WithRepository a -> IO a
runWithRepo repo action = runReaderT (getWithRepository action) repo

-- | Compute path to a file inside .git folder (e.g., repoFile repo ["objects", "4b"])
gitPath :: [FilePath] -> WithRepository FilePath
gitPath path = do
  gitdir <- asks repoGitdir
  return $ foldl' (</>) gitdir path

worktreePath :: [FilePath] -> WithRepository FilePath
worktreePath path = do
  worktree <- asks repoWorktree
  return $ foldl' (</>) worktree path

worktreePath' :: FilePath -> WithRepository FilePath
worktreePath' path = do
  worktree <- asks repoWorktree
  return $ worktree </> path

objectsPath :: [FilePath] -> WithRepository FilePath
objectsPath path = gitPath ("objects" : path)

packPath :: [FilePath] -> WithRepository FilePath
packPath path = objectsPath ("pack" : path)

headsPath :: FilePath -> WithRepository FilePath
headsPath head' = gitPath ["refs", "heads", head']

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

makeRepo :: (MonadIO m) => FilePath -> FilePath -> m (Repository)
makeRepo worktree gitdir = do
  pcIndexFiles <- newIORef Nothing
  pcIndexes <- newIORef Map.empty
  return $ Repository{repoWorktree = worktree, repoGitdir = gitdir, repoPackCache = PackCache{..}}

openRepo :: FilePath -> IO (Maybe Repository)
openRepo worktree = do
  let gitdir = worktree </> ".git"
  isDir <- doesDirectoryExist gitdir
  if isDir
    then Just <$> makeRepo worktree gitdir
    else return Nothing

type WorkTreePath = FilePath

toWorktreePath :: FilePath -> WithRepository (FilePath, WorkTreePath)
toWorktreePath path = do
  fullpath <- Dir.canonicalizePath path
  worktreePrefix <- asks repoWorktree
  let relative = makeRelative worktreePrefix fullpath
  return $ case (relative, worktreePrefix `isPrefixOf` fullpath) of
    (".", _) -> (fullpath, "")
    (_, False) -> throwStrErr "toWorktreePath" $ "path not in worktree: " <> path
    _ -> (fullpath, relative)
