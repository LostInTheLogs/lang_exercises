{-# LANGUAGE RecordWildCards #-}

module HGit.GitLsFiles (gitLsFiles, LsFilesOptions (..)) where

import HGit.Index (Index (..), IndexEntry (..), readIndex)
import HGit.Object (Hash, ObjType (CommitObj), findObject, objPayload, readObj, strToHash)
import HGit.Repository (Repository, getRepo)

data LsFilesOptions = LsFilesOptions {}

gitLsFiles :: LsFilesOptions -> IO ()
gitLsFiles LsFilesOptions{..} = do
  repo <- getRepo
  Index{..} <- readIndex repo
  mapM_ lsFile idxEntries

lsFile :: IndexEntry -> IO ()
lsFile IndexEntry{..} = do
  putStrLn iePath
