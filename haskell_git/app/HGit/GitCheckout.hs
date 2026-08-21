{-# LANGUAGE ViewPatterns #-}

module HGit.GitCheckout (gitCheckout, CheckoutOptions (..)) where

import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceObj, findBranch, findObject)
import HGit.GitDiffIndex (IndexTreeDiff, diffTreeIndex, treeIndexDiffToPath)
import HGit.Index (Index (idxEntries), IndexEntries, IndexEntry (iePath), isEntryModified, readIndex)
import HGit.Object (ObjType (..))
import HGit.Repository (WithRepository (WithRepository), WorkTreePath, gitPath, runWithFoundRepo, worktreePath')
import HGit.Tree (flattenTree, objToTree)
import HGit.Utils (throwErr)
import Relude
import qualified UnliftIO.Directory as Dir

-- error: The following untracked working tree files would be overwritten by checkout:
-- 	haskell_git/test
-- Please move or remove them before you switch branches.
-- Aborting

-- | Returns untracked files that would be overwritten by checkout
findUntrackedConflicts :: [WorkTreePath] -> IndexEntries -> WithRepository [WorkTreePath]
findUntrackedConflicts files' entries' = work files' entries' []
 where
  work :: [WorkTreePath] -> IndexEntries -> [WorkTreePath] -> WithRepository [WorkTreePath]

  -- only in files, untracked
  work files (V.null -> True) acc = do
    newAcc <- filterM doesConflict files
    return $ reverse acc ++ newAcc
  -- only in index, tracked
  work [] _ acc = do
    return acc
  work files@(file : filesTail) entries acc = do
    let entry = V.unsafeHead entries
    case compare file (iePath entry) of
      -- only in files, untracked
      LT -> do
        conflict <- doesConflict file
        let newAcc = if conflict then file : acc else acc
        work filesTail entries newAcc
      -- in both, tracked
      EQ -> do
        work filesTail (V.unsafeTail entries) acc
      -- only in index, tracked
      GT -> do
        work files (V.unsafeTail entries) acc

  doesConflict relPath = do
    path <- worktreePath' relPath
    Dir.doesFileExist path

findTrackedConflicts :: IndexEntries -> WithRepository [WorkTreePath]
findTrackedConflicts entries = do
  tree <- objToTree <$> findAndCoerceObj TreeObj "HEAD"
  (treeIndexDiffToPath <$>) <$> diffTreeIndex tree entries False

data CheckoutOptions = CheckoutOptions {optBranch :: Text}

gitCheckout :: CheckoutOptions -> IO ()
gitCheckout CheckoutOptions{..} = runWithFoundRepo $ do
  tree <- objToTree <$> findAndCoerceObj TreeObj optBranch
  flattened <- flattenTree tree

  idx <- readIndex

  untrackedConflicts <- findUntrackedConflicts (fst <$> flattened) (idxEntries idx)
  trackedConflicts <- findTrackedConflicts (idxEntries idx)

  unless (null trackedConflicts) $ do
    putTextLn "error: Your local changes to the following files would be overwritten by checkout:"
    forM_ trackedConflicts $ \x -> putStrLn ("\t" <> x)
    putTextLn "Please commit your changes or stash them before you switch branches."
    putTextLn ""

  unless (null untrackedConflicts) $ do
    putTextLn "error: The following untracked working tree files would be overwritten by checkout:"
    forM_ untrackedConflicts $ \x -> putStrLn ("\t" <> x)
    putTextLn "Please move or remove them before you switch branches."
    putTextLn ""

  unless (null trackedConflicts && null untrackedConflicts) $ do
    putTextLn "Aborting"
    exitFailure

  -- TODO: git reset hard
  return ()
