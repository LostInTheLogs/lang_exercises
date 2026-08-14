{-# LANGUAGE RecordWildCards #-}

module HGit.GitLsFiles (gitLsFiles, LsFilesOptions (..)) where

import Control.Monad (filterM)
import HGit.Index (Index (..), IndexEntry (..), isEntryModified, readIndex)
import HGit.Object (Hash, ObjType (CommitObj), findObject, objPayload, readObj, strToHash)
import HGit.Repository (Repository, getRepo)

data LsFilesOptions = LsFilesOptions {optModified :: Bool}

gitLsFiles :: LsFilesOptions -> IO ()
gitLsFiles LsFilesOptions{..} = do
  repo <- getRepo
  Index{..} <- readIndex repo
  entries <-
    if optModified
      then filterM (isEntryModified repo) idxEntries
      else return idxEntries
  mapM_ lsFile entries

lsFile :: IndexEntry -> IO ()
lsFile IndexEntry{..} = do
  putStrLn iePath
