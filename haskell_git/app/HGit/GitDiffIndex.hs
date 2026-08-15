{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HGit.GitDiffIndex (gitDiffIndex, DiffIndexOptions (..)) where

{-
doesn't compare files not in index

If Path exists in Index but not in Tree → Added (A)
If Path exists in Tree but not in Index → Deleted (D)
If Path exists in both (M):
  --cached : compare Hash from Index and Tree
  else: index entry unmodified do --cached else hash the file
-}

--

import Control.Monad (filterM)
import Data.Map as Map
import HGit.Index (Index (..), IndexEntry (..), getEntryHash, getStatData, isEntryModified, readIndex)
import HGit.Object (Hash, ObjType (CommitObj, TreeObj), findObject, getFileHash, objPayload, readObj, strToHash)
import HGit.ObjectCoerce (findAndCoerceObj)
import HGit.Repository (Repository, getRepo, worktreePath)
import HGit.Tree (Tree, TreeItem (..), flattenTree, objToTree)
import qualified System.Directory as Dir

data DiffIndexOptions = DiffIndexOptions {optTree :: String, optCached :: Bool}

data IndexDiff = Added IndexEntry | Deleted (FilePath, TreeItem) | Modified TreeItem IndexEntry deriving (Show)

diffTreeIndex :: Repository -> Map FilePath TreeItem -> [IndexEntry] -> Bool -> IO [IndexDiff]
diffTreeIndex repo treeMap' idxEntries cached = reverse <$> doDiff treeMap' idxEntries []
 where
  doDiff :: Map.Map FilePath TreeItem -> [IndexEntry] -> [IndexDiff] -> IO [IndexDiff]
  doDiff (Map.null -> True) rest acc = return $ (Added <$> rest) ++ acc
  doDiff treeMap [] acc = return $ (Deleted <$> Map.toList treeMap) ++ acc
  doDiff treeMap (entry : rest) acc = do
    case treeMap Map.!? iePath entry of
      Just treeItem -> do
        let newMap = Map.delete (iePath entry) treeMap
        doDiff newMap rest =<< handleDiff treeItem entry acc
      Nothing -> doDiff treeMap rest (Added entry : acc)

  handleDiff :: TreeItem -> IndexEntry -> [IndexDiff] -> IO [IndexDiff]
  handleDiff treeItem entry acc = do
    newHash <- if cached then return $ Just $ ieObjHash entry else getEntryHash repo entry
    case newHash of
      Just hash ->
        if tiHash treeItem == hash
          then return acc
          else return $ Modified treeItem entry : acc
      Nothing -> return $ Deleted (iePath entry, treeItem) : acc

printDiff :: IndexDiff -> IO ()
printDiff diff = case diff of
  Added entry -> do
    putStr "A     "
    putStrLn $ iePath entry
  Deleted (path, _item) -> do
    putStr "D     "
    putStrLn path
  Modified _item entry -> do
    putStr "M     "
    putStrLn $ iePath entry

gitDiffIndex :: DiffIndexOptions -> IO ()
gitDiffIndex DiffIndexOptions{..} = do
  repo <- getRepo
  idxEntries <- idxEntries <$> readIndex repo
  flattenedTree <- flattenTree repo . objToTree =<< findAndCoerceObj repo TreeObj optTree
  let treeMap = Map.fromList flattenedTree
  diffs <- diffTreeIndex repo treeMap idxEntries optCached
  mapM_ printDiff diffs
