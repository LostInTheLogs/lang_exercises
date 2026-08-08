module HGit.Repository (
  Repository (..),
  repoPath,
  getRepo,
  findRepo,
) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath
import System.IO.Error (alreadyExistsErrorType, doesNotExistErrorType, mkIOError)

data Repository = Repository
  { repoWorktree :: FilePath
  , repoGitdir :: FilePath -- Path to .git directory
  }
  deriving (Show, Eq)

-- | Compute path to a file inside .git folder (e.g., repoFile repo ["objects", "4b"])
repoPath :: Repository -> [FilePath] -> FilePath
repoPath repo = foldl (</>) (repoGitdir repo)

findRepo' :: FilePath -> IO (Maybe Repository)
findRepo' from =
  do
    here <- canonicalizePath from
    runMaybeT $
      MaybeT (openRepo here) <|> do
        let parent = takeDirectory here
        guard (parent /= here)
        MaybeT (findRepo' parent)

findRepo :: IO (Maybe Repository)
findRepo = findRepo' "."

getRepo :: IO Repository
getRepo = do
  repo <- findRepo' "."
  case repo of
    Nothing -> throwIO $ mkIOError doesNotExistErrorType "getRepo" Nothing (Just ".git")
    Just a -> return a

openRepo :: FilePath -> IO (Maybe Repository)
openRepo worktree = do
  let gitdir = worktree </> ".git"
  isDir <- doesDirectoryExist gitdir
  if isDir
    then
      return $ Just Repository{repoWorktree = worktree, repoGitdir = gitdir}
    else return Nothing
