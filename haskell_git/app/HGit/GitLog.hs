{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.GitLog (gitLog, LogOptions (..)) where

import qualified Data.Set as Set
import qualified Data.Text as T
import HGit.Commit (Commit (..), readCommit)
import HGit.FindObject (findObject)
import HGit.Object (Hash, ObjType (CommitObj), objPayload, readObj, strToHash)
import HGit.Repository (Repository, WithRepository, runWithFoundRepo)
import Relude

data LogOptions = LogOptions {optRef :: Text}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = runWithFoundRepo $ do
  rootHash <- findObject optRef
  logRec [rootHash] Set.empty

logRec :: [Hash] -> Set.Set Hash -> WithRepository ()
logRec [] _ = pass
logRec (hash : hashes) seen =
  if Set.member hash seen
    then
      logRec hashes seen
    else do
      commit <- readCommit hash
      putTextLn $ oneLine commit
      logRec (hashes ++ commitParents commit) (Set.insert hash seen)

oneLine :: Commit -> Text
oneLine Commit{..} = do
  let hash = toText $ take 7 (show commitHash)
  let msg = decodeUtf8 commitMsg
  let oneLnMsg = T.strip $ T.replace "\n" " " msg
  hash <> " " <> oneLnMsg
