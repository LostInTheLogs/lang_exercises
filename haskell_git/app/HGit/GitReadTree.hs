{-# LANGUAGE RecordWildCards #-}

module HGit.GitReadTree (gitReadTree, ReadTreeOptions (..)) where

import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceObj, findObject)
import HGit.Index (Index (idxEntries), IndexEntry, makeBlankEntry, readIndex, writeIndex)
import HGit.Object (ObjType (TreeObj))
import HGit.Repository (WithRepository, gitPath, runWithFoundRepo)
import HGit.Tree (Tree, TreeItem (..), flattenTree, objToTree)
import HGit.Utils (throwErr)
import Relude
import qualified UnliftIO.Directory as Dir

data ReadTreeOptions = ReadTreeOptions {optTree :: Text}

gitReadTree :: ReadTreeOptions -> IO ()
gitReadTree ReadTreeOptions{..} = runWithFoundRepo $ do
  tree <- objToTree <$> findAndCoerceObj TreeObj optTree

  newEntries <- entriesFromTree tree

  index <- readIndex
  let newIdx = index{idxEntries = newEntries}

  writeIndex $! newIdx

entriesFromTree :: Tree -> WithRepository (V.Vector IndexEntry)
entriesFromTree tree = do
  flattened <- flattenTree tree

  let newEntries = flip V.map (V.fromList flattened) $ \(path, item) -> do
        makeBlankEntry path (tiHash item) (tiMode item)

  return newEntries
