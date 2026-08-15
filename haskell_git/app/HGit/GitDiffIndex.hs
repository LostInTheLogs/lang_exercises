{-# LANGUAGE RecordWildCards #-}

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
import HGit.Index (Index (..), IndexEntry (..), isEntryModified, readIndex)
import HGit.Object (Hash, ObjType (CommitObj, TreeObj), findObject, objPayload, readObj, strToHash)
import HGit.ObjectCoerce (findAndCoerceObj)
import HGit.Repository (Repository, getRepo)
import HGit.Tree (objToTree)

data DiffIndexOptions = DiffIndexOptions {optTree :: String, optCached :: Bool}

gitDiffIndex :: DiffIndexOptions -> IO ()
gitDiffIndex DiffIndexOptions{..} = do
    repo <- getRepo
    Index{..} <- readIndex repo
    tree <- objToTree <$> findAndCoerceObj repo TreeObj optTree
    -- entries <-
    --     if optModified
    --         then filterM (isEntryModified repo) idxEntries
    --         else return idxEntries
    mapM_ lsFile idxEntries

lsFile :: IndexEntry -> IO ()
lsFile IndexEntry{..} = do
    putStrLn iePath
