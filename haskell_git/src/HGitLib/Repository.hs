module HGitLib.Repository (Repository (..), repoPath) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import System.Directory (doesDirectoryExist)
import System.FilePath

data Repository = Repository
  { repoWorktree :: FilePath
  , repoGitdir :: FilePath -- Path to .git directory
  }
  deriving (Show, Eq)

-- | Compute path to a file inside .git folder (e.g., repoFile repo ["objects", "4b"])
repoPath :: Repository -> [FilePath] -> FilePath
repoPath repo = foldl (</>) (repoGitdir repo)

findRepo :: FilePath -> IO (Maybe Repository)
findRepo here =
  runMaybeT $
    MaybeT (openRepo here) <|> do
      let parent = takeDirectory here
      guard (parent /= here)
      MaybeT (findRepo parent)

openRepo :: FilePath -> IO (Maybe Repository)
openRepo worktree = do
  let gitdir = worktree </> ".git"
  isDir <- doesDirectoryExist gitdir
  if isDir
    then
      return $ Just Repository{repoWorktree = worktree, repoGitdir = gitdir}
    else return Nothing
