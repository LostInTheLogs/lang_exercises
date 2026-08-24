module HGit.GitCheckout (gitCheckout, CheckoutOptions (..)) where

import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceToTree)
import HGit.GitDiffIndex (diffTreeIndex)
import HGit.Index (Index (idxEntries), IndexEntries, IndexEntry (iePath), TreeIndexDiff (..), TreeIndexFold (..), isEntryModified, readIndex, treeIndexFoldlM)
import HGit.Repository (WithRepository (WithRepository), WorkTreePath, gitPath, runWithFoundRepo, worktreePath')
import HGit.Tree (FlattenedTree, flattenTree, objToTree)
import HGit.Utils (throwErr)
import Relude
import qualified UnliftIO.Directory as Dir

-- | Returns untracked files that would be overwritten by checkout
findUntrackedConflicts :: FlattenedTree -> IndexEntries -> WithRepository [WorkTreePath]
findUntrackedConflicts tree entries =
  treeIndexFoldlM tree entries [] $ \acc x -> case x of
    OnlyInTree (path, _) -> do
      conflict <- Dir.doesFileExist =<< worktreePath' path
      return $ if conflict then path : acc else acc
    _ -> return acc

findTrackedConflicts :: IndexEntries -> WithRepository [TreeIndexDiff]
findTrackedConflicts entries = do
  tree <- flattenTree =<< findAndCoerceToTree "HEAD"
  diffTreeIndex tree entries False

data CheckoutOptions = CheckoutOptions {optBranch :: Text}

gitCheckout :: CheckoutOptions -> IO ()
gitCheckout CheckoutOptions{..} = runWithFoundRepo $ do
  tree <- findAndCoerceToTree optBranch
  flattened <- flattenTree tree
  idx <- readIndex

  untrackedConflicts <- findUntrackedConflicts flattened (idxEntries idx)
  trackedConflicts <- findTrackedConflicts (idxEntries idx)

  unless (null trackedConflicts) $ do
    putTextLn "error: Your local changes to the following files would be overwritten by checkout:"
    forM_ (formatTrackedConflict <$> trackedConflicts) $ \x -> putStrLn ("\t" <> x)
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
  pass

formatTrackedConflict :: TreeIndexDiff -> WorkTreePath
formatTrackedConflict x =
  case x of
    DiffOnlyInIndex entry -> iePath entry
    DiffOnlyInTree (path, _) -> path
    DiffModified _ entry -> iePath entry
    _ -> error "unexpected TreeIndexDiff"
