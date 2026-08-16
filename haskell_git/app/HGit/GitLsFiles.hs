{-# LANGUAGE RecordWildCards #-}

module HGit.GitLsFiles (gitLsFiles, LsFilesOptions (..)) where

import Control.Monad (filterM)
import qualified Data.Vector.Strict as V
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
      then V.filterM (isEntryModified repo) idxEntries
      else return idxEntries
  mapM_ lsFile entries

lsFile :: IndexEntry -> IO ()
lsFile IndexEntry{..} = do
  putStrLn iePath
