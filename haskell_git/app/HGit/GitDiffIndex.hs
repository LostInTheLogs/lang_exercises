module HGit.GitDiffIndex (gitDiffIndex, DiffIndexOptions (..), IndexTreeDiff (..), diffTreeIndex) where

import Data.Map as Map
import qualified Data.Vector as V
import HGit.Index (Index (..), IndexEntries, IndexEntry (..), getEntryHash, getStatData, isEntryModified, readIndex)
import HGit.Object (Hash, ObjType (CommitObj, TreeObj), findObject, getFileHash, objPayload, readObj, strToHash)
import HGit.ObjectCoerce (findAndCoerceObj)
import HGit.Repository (Repository, WithRepository, runWithFoundRepo, worktreePath)
import HGit.Tree (Tree, TreeItem (..), flattenTree, objToTree)
import Relude
import qualified UnliftIO.Directory as Dir

data DiffIndexOptions = DiffIndexOptions {optTree :: Text, optCached :: Bool}

data IndexTreeDiff = ITDAdded IndexEntry | ITDDeleted (FilePath, TreeItem) | ITDModified TreeItem IndexEntry deriving (Show)

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
  tree <- objToTree <$> findAndCoerceObj TreeObj optTree
  diffs <- diffTreeIndex tree idxEntries optCached
  mapM_ printDiff diffs

diffTreeIndex :: Tree -> IndexEntries -> Bool -> WithRepository [IndexTreeDiff]
diffTreeIndex tree idxEntries cached = do
  flattenedTree <- flattenTree tree
  work (Map.fromList flattenedTree) 0 []
 where
  work :: Map.Map FilePath TreeItem -> Int -> [IndexTreeDiff] -> WithRepository [IndexTreeDiff]
  work treeMap idx acc
    | Map.null treeMap =
        let rest = V.drop idx idxEntries
         in return $ reverse acc ++ V.toList (ITDAdded <$> rest)
    | idx >= V.length idxEntries =
        return $ reverse acc ++ (ITDDeleted <$> Map.toList treeMap)
    | otherwise = do
        let entry = idxEntries V.! idx
        case treeMap Map.!? iePath entry of
          Just treeItem -> do
            let newMap = Map.delete (iePath entry) treeMap
            work newMap (idx + 1) =<< handleDiff treeItem entry acc
          Nothing ->
            work treeMap (idx + 1) (ITDAdded entry : acc)

  handleDiff :: TreeItem -> IndexEntry -> [IndexTreeDiff] -> WithRepository [IndexTreeDiff]
  handleDiff treeItem entry acc = do
    newHash <- if cached then return $ Just $ ieObjHash entry else getEntryHash entry
    case newHash of
      Just hash ->
        if tiHash treeItem == hash
          then return acc
          else return $ ITDModified treeItem entry : acc
      Nothing -> return $ ITDDeleted (iePath entry, treeItem) : acc

printDiff :: (MonadIO m) => IndexTreeDiff -> m ()
printDiff diff = liftIO $ case diff of
  ITDAdded entry -> do
    putStr "A     "
    putStrLn $ iePath entry
  ITDDeleted (path, _item) -> do
    putStr "D     "
    putStrLn path
  ITDModified _item entry -> do
    putStr "M     "
    putStrLn $ iePath entry
