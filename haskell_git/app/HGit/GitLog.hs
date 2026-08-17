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
import HGit.Commit (Commit (..), readCommit)
import HGit.Object (Hash, ObjType (CommitObj), findObject, objPayload, readObj, strToHash)
import HGit.Repository (Repository, WithRepository, runWithFoundRepo)
import Relude

data LogOptions = LogOptions {optRef :: Text}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = runWithFoundRepo $ do
  rootHash <- findObject optRef
  logRec [rootHash] Set.empty

logRec :: [Hash] -> Set.Set Hash -> WithRepository ()
logRec [] _ = return ()
logRec (hash : hashes) seen =
  if Set.member hash seen
    then
      logRec hashes seen
    else do
      commit <- readCommit hash
      putTextLn $ oneLine commit
      logRec (hashes ++ commitParents commit) (Set.insert hash seen)

oneLine :: Commit -> T.Text
oneLine Commit{..} = do
  let hash = T.pack $ take 7 (show commitHash)
  let msg = TE.decodeUtf8 commitMsg
  let oneLnMsg = T.strip $ T.replace "\n" " " msg
  hash <> " " <> oneLnMsg
