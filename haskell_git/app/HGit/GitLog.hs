{-# LANGUAGE RecordWildCards #-}

module HGit.GitLog (gitLog, LogOptions (..)) where

import qualified Data.Set as Set
import qualified Data.Text as T
import HGit.Commit (Commit (..), CommitQueue, cmtQueuePop, makeCmtQueue, oneLineShort, readCommit)
import HGit.FindObject (findObject)
import HGit.Repository (Repository, WithRepository, runWithFoundRepo)
import Relude

data LogOptions = LogOptions {optRef :: Text}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = runWithFoundRepo $ do
  rootCmt <- readCommit =<< findObject optRef
  let queue = makeCmtQueue [rootCmt]
  logRec =<< cmtQueuePop queue

logRec :: (Maybe Commit, CommitQueue) -> WithRepository ()
logRec (Nothing, _) = pass
logRec (Just commit, queue) = do
  putTextLn $ oneLineShort commit
  logRec =<< cmtQueuePop queue
