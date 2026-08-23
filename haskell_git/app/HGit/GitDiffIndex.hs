module HGit.GitDiffIndex (
  gitDiffIndex,
  DiffIndexOptions (..),
  diffTreeIndex,
) where

import Data.Map as Map
import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceObj)
import HGit.Index (Index (..), IndexEntries, IndexEntry (..), TreeIndexDiff (..), getEntryHash, getStatData, isEntryModified, readIndex, treeIndexDiffFoldlM)
import HGit.Object (Hash, ObjType (CommitObj, TreeObj), getFileHash, objPayload, readObj, strToHash)
import HGit.Repository (Repository, WithRepository, WorkTreePath, runWithFoundRepo, worktreePath)
import HGit.Tree (FlattenedTree, Tree, TreeItem (..), flattenTree, objToTree)
import Relude
import qualified UnliftIO.Directory as Dir

data DiffIndexOptions = DiffIndexOptions {optTree :: Text, optCached :: Bool}

{-
doesn't compare files not in index

If Path exists in Index but not in Tree → Added (A)
If Path exists in Tree but not in Index → Deleted (D)
If Path exists in both (M):
  --cached : compare Hash from Index and Tree
  else: index entry unmodified do --cached else hash the file
-}

gitDiffIndex :: DiffIndexOptions -> IO ()
gitDiffIndex DiffIndexOptions{..} = runWithFoundRepo $ do
  idxEntries <- idxEntries <$> readIndex
  tree <- flattenTree . objToTree =<< findAndCoerceObj TreeObj optTree
  diffs <- diffTreeIndex tree idxEntries optCached
  mapM_ printDiff diffs

diffTreeIndex :: FlattenedTree -> IndexEntries -> Bool -> WithRepository [TreeIndexDiff]
diffTreeIndex tree idxEntries cached =
  reverse <$> do
    treeIndexDiffFoldlM tree idxEntries cached [] $ \acc x ->
      case x of
        DiffSame _ _ -> return acc
        _ -> return $ x : acc

printDiff :: (MonadIO m) => TreeIndexDiff -> m ()
printDiff diff = liftIO $ case diff of
  DiffOnlyInIndex entry -> do
    putStr "A     "
    putStrLn $ iePath entry
  DiffOnlyInTree (path, _item) -> do
    putStr "D     "
    putStrLn path
  DiffModified _item entry -> do
    putStr "M     "
    putStrLn $ iePath entry
  _ -> error "unexpected TreeIndexDiff"
