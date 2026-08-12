{-# LANGUAGE RecordWildCards #-}

module HGit.GitLog (gitLog, LogOptions (..)) where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import HGit.Commit (Commit (..), parseCommit, readCommit)
import HGit.Object (ObjType (CommitObj), findObject, hashToStr, objPayload, readObj, strToHash)
import HGit.Repository (Repository, getRepo)

data LogOptions = LogOptions {optRef :: String}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = do
  putStrLn "log"
  repo <- getRepo
  rootHash <- findObject repo optRef
  logRec repo [rootHash] Set.empty

logRec :: Repository -> [BS.ByteString] -> Set.Set BS.ByteString -> IO ()
logRec _ [] seen = return ()
logRec repo (hash : hashes) seen =
  if Set.member hash seen
    then
      logRec repo hashes seen
    else do
      commit <- readCommit repo hash
      putStrLn $ hashToStr hash
      logRec repo (hashes ++ commitParents commit) (Set.insert hash seen)
