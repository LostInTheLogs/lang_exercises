module HGit.GitStatus (StatusOptions (..), gitStatus) where

import Data.List (stripPrefix)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceObj)
import HGit.GitDiffIndex (IndexTreeDiff (..), diffTreeIndex)
import HGit.Ignore (listRepoFilesRecursive)
import HGit.Index (EntryStatus (..), IndexEntries, IndexEntry (..), getEntryHash, getEntryStatus, idxEntries, isEntryModified, readIndex)
import HGit.Object (ObjType (..))
import HGit.Repository (Repository (repoWorktree), WithRepository, gitPath, runWithFoundRepo, worktreePath)
import HGit.Tree (objToTree)
import HGit.Utils (fReadStrLine, throwErr)
import Relude

data StatusOptions = StatusOptions {}

getStagedChanges :: IndexEntries -> WithRepository [IndexTreeDiff]
getStagedChanges entries = do
  tree <- objToTree <$> findAndCoerceObj TreeObj "HEAD"
  diffTreeIndex tree entries True

gitStatus :: StatusOptions -> IO ()
gitStatus StatusOptions{} = runWithFoundRepo $ do
  branch <- getBranch
  putStrLn $ "On branch " <> branch
  putStrLn ""

  entries <- idxEntries <$> readIndex
  staged <- getStagedChanges entries
  unless (null staged) $ do
    putStrLn "Changes to be committed:"
    putStrLn "  (use \"git restore --staged <file>...\" to unstage)"
    liftIO $ mapM_ printStaged staged
    putStrLn ""

  unstaged <- V.mapMaybeM getEntryStatus entries
  unless (null unstaged) $ do
    putStrLn "Changes not staged for commit:"
    putStrLn "  (use \"git add <file>...\" to update what will be committed)"
    putStrLn "  (use \"git restore <file>...\" to discard changes in working directory)"
    liftIO $ mapM_ printUnstaged unstaged
    putStrLn ""

  untracked <- getUntracked entries
  unless (null untracked) $ do
    putStrLn "Untracked files:"
    putStrLn "  (use \"git add <file>...\" to include in what will be committed)"
    liftIO $ mapM_ printUntracked untracked
    putStrLn ""

  when (null staged && null unstaged) $ putStrLn "nothing to commit, working tree clean"
  when (null staged && not (null unstaged)) $ putStrLn "no changes added to commit (use \"git add\" and/or \"git commit -a\")"

printStaged :: IndexTreeDiff -> IO ()
printStaged diff =
  liftIO $
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

printUntracked :: FilePath -> IO ()
printUntracked path = do
  putStr "  \t"
  putStrLn path

getBranch :: WithRepository String
getBranch = do
  refOrHead <- fReadStrLine =<< gitPath ["HEAD"]
  case stripPrefix "ref: refs/heads/" refOrHead of
    Nothing -> return refOrHead
    Just ref -> return ref

getUntracked :: IndexEntries -> WithRepository (Set.Set FilePath)
getUntracked entries = do
  let tracked = Set.fromList $ V.toList $ iePath <$> entries
  files <- Set.fromList <$> listRepoFilesRecursive ""
  return $ Set.difference files tracked
