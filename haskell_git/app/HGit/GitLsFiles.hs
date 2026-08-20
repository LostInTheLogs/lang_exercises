{-# LANGUAGE RecordWildCards #-}

module HGit.GitLsFiles (gitLsFiles, LsFilesOptions (..)) where

import Control.Monad (filterM)
import qualified Data.Vector as V
import HGit.Index (Index (..), IndexEntry (..), isEntryModified, readIndex)
import HGit.Object (Hash, ObjType (CommitObj), objPayload, readObj, strToHash)
import HGit.Repository (Repository, runWithFoundRepo)
import Relude

data LsFilesOptions = LsFilesOptions {optModified :: Bool}

gitLsFiles :: LsFilesOptions -> IO ()
gitLsFiles LsFilesOptions{..} = runWithFoundRepo $ do
  Index{..} <- readIndex
  entries <-
    if optModified
      then V.filterM (isEntryModified) idxEntries
      else return idxEntries
  mapM_ lsFile entries

lsFile :: (MonadIO m) => IndexEntry -> m ()
lsFile IndexEntry{..} = liftIO $ do
  putStrLn iePath
