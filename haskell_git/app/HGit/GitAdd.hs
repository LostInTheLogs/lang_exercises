{-# LANGUAGE RecordWildCards #-}

module HGit.GitAdd (gitAdd, AddOptions (..)) where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector.Strict as V
import HGit.Ignore (listRepoFilesRecursive)
import HGit.Index (Index (..), IndexEntry (..), fileToEntry, makeEntry, readIndex, writeIndex)
import HGit.Object (ObjType (BlobObj), Object (objHash), writeObj)
import HGit.ObjectType (makeObject)
import HGit.Repository (Repository (..), gitPath, runWithFoundRepo, worktreePath')
import HGit.Utils (insertManySorted, throwErr, throwStrErr)
import Relude
import System.FilePath (pathSeparator)
import qualified UnliftIO.Directory as Dir

data AddOptions = AddOptions {optPath :: FilePath}

gitAdd :: AddOptions -> IO ()
gitAdd AddOptions{..} = runWithFoundRepo $ do
  path <- Dir.canonicalizePath optPath
  worktreePath <- asks repoWorktree
  let relPath = case List.stripPrefix worktreePath path of
        Just a -> fromMaybe a $ List.stripPrefix [pathSeparator] a
        Nothing -> throwErr "gitAdd" "path not in worktree"

  idx <- readIndex

  fileExists <- Dir.doesFileExist path
  dirExists <- Dir.doesDirectoryExist path
  let doesNotExist = throwStrErr "gitAdd" $ "The file does not exist: " <> optPath
  newFiles <-
    if fileExists
      then do
        return $ List.singleton relPath
      else if dirExists then listRepoFilesRecursive relPath else doesNotExist

  let newFilesSet = Set.fromList newFiles
  let entriesSet = idxEntries idx
  let oldEntries = V.filter (\entry -> not (iePath entry `Set.member` newFilesSet)) $ idxEntries idx

  newEntries <- forM newFiles $ \a -> do
    filePath <- worktreePath' a
    contents <- readFileLBS filePath
    let obj = makeObject contents BlobObj
    writeObj obj
    makeEntry a filePath (objHash obj)

  let allEntries = insertManySorted oldEntries (V.fromList newEntries)
  let newIdx = idx{idxEntries = allEntries}

  writeIndex $! newIdx
