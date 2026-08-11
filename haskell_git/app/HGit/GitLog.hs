{-# LANGUAGE RecordWildCards #-}

module HGit.GitLog (gitLog, LogOptions (..)) where

import qualified Data.ByteString as BS
import HGit.Commit (Commit (..), parseCommit, readCommit)
import HGit.Object (ObjType (CommitObj), findObject, hashToStr, objPayload, readObj, strToHash)
import HGit.Repository (Repository, getRepo)

data LogOptions = LogOptions {optRef :: String}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = do
  putStrLn "log"
  repo <- getRepo
  rootHash <- findObject repo optRef
  logRec repo [rootHash]

logRec :: Repository -> [BS.ByteString] -> IO ()
logRec _ [] = return ()
logRec repo (hash : hashes) = do
  commit <- readCommit repo hash
  putStrLn $ hashToStr hash
  logRec repo (hashes ++ commitParents commit)
