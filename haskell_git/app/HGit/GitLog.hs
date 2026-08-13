{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.GitLog (gitLog, LogOptions (..)) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import HGit.Commit (Commit (..), parseCommit, readCommit)
import HGit.Object (ObjType (CommitObj), findObject, hashToStr, objPayload, readObj, strToHash)
import HGit.Repository (Repository, getRepo)

data LogOptions = LogOptions {optRef :: String}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = do
  repo <- getRepo
  rootHash <- findObject repo optRef
  logRec repo [rootHash] Set.empty

logRec :: Repository -> [BS.ByteString] -> Set.Set BS.ByteString -> IO ()
logRec _ [] _ = return ()
logRec repo (hash : hashes) seen =
  if Set.member hash seen
    then
      logRec repo hashes seen
    else do
      commit <- readCommit repo hash
      TIO.putStrLn $ oneLine commit
      logRec repo (hashes ++ commitParents commit) (Set.insert hash seen)

oneLine :: Commit -> T.Text
oneLine Commit{..} = do
  let hash = T.pack $ take 7 (hashToStr commitHash)
  let msg = TE.decodeUtf8 commitMsg
  let oneLnMsg = T.strip $ T.replace "\n" " " msg
  hash <> " " <> oneLnMsg
