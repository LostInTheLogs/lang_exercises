module HGit.GitStatus (StatusOptions (..), gitStatus) where

import Control.Monad (filterM, unless, when)
import Data.List (stripPrefix)
import qualified Data.Vector.Strict as V
import HGit.GitDiffIndex (IndexTreeDiff (..), diffTreeIndex)
import HGit.Index (EntryStatus (..), IndexEntries, IndexEntry (..), getEntryHash, getEntryStatus, idxEntries, isEntryModified, readIndex)
import HGit.Object (ObjType (..))
import HGit.ObjectCoerce (findAndCoerceObj)
import HGit.Repository (Repository, getRepo, repoPath)
import HGit.Tree (objToTree)
import HGit.Utils (fReadLine)

data StatusOptions = StatusOptions {}

getStagedChanges :: Repository -> IndexEntries -> IO [IndexTreeDiff]
getStagedChanges repo entries = do
  tree <- objToTree <$> findAndCoerceObj repo TreeObj "HEAD"
  diffTreeIndex repo tree entries True

gitStatus :: StatusOptions -> IO ()
gitStatus StatusOptions{} = do
  repo <- getRepo
  branch <- getBranch repo
  putStrLn $ "On branch " ++ branch
  putStrLn ""

  entries <- idxEntries <$> readIndex repo
  staged <- getStagedChanges repo entries
  unless (null staged) $ do
    putStrLn "Changes to be committed:"
    putStrLn "  (use \"git restore --staged <file>...\" to unstage)"
    mapM_ printStaged staged
    putStrLn ""

  unstaged <- V.mapMaybeM (getEntryStatus repo) entries
  unless (null unstaged) $ do
    putStrLn "Changes not staged for commit:"
    putStrLn "  (use \"git add <file>...\" to update what will be committed)"
    putStrLn "  (use \"git restore <file>...\" to discard changes in working directory)"
    mapM_ printUnstaged unstaged
    putStrLn ""

  when (null staged && null unstaged) $ putStrLn "nothing to commit, working tree clean"
  when (null staged && not (null unstaged)) $ putStrLn "no changes added to commit (use \"git add\" and/or \"git commit -a\")"

-- TODO:
-- Untracked files:
--   (use "git add <file>..." to include in what will be committed)

printStaged :: IndexTreeDiff -> IO ()
printStaged diff =
  putStr "  \t" *> case diff of
    ITDAdded entry -> do
      putStr "new file:   "
      putStrLn $ iePath entry
    ITDDeleted (path, _item) -> do
      putStr "deleted:    "
      putStrLn path
    ITDModified _item entry -> do
      putStr "modified:   "
      putStrLn $ iePath entry

printUnstaged :: (EntryStatus, IndexEntry) -> IO ()
printUnstaged (status, entry) = do
  putStr "  \t"
  putStr $ case status of
    EntryModified -> "modified:   "
    EntryDeleted -> "deleted:    "
  putStrLn $ iePath entry

getBranch :: Repository -> IO String
getBranch repo = do
  refOrHead <- fReadLine $ repoPath repo ["HEAD"]
  case stripPrefix "ref: refs/heads/" refOrHead of
    Nothing -> return refOrHead
    Just ref -> return ref
