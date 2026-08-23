{-# LANGUAGE RecordWildCards #-}

module HGit.GitReadTree (gitReadTree, ReadTreeOptions (..)) where

import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceToTree)
import HGit.Index (Index (idxEntries), IndexEntry, makeBlankEntry, readIndex, writeIndex)
import HGit.Repository (WithRepository, gitPath, runWithFoundRepo)
import HGit.Tree (Tree, TreeItem (..), flattenTree)
import Relude

data ReadTreeOptions = ReadTreeOptions {optTree :: Text}

gitReadTree :: ReadTreeOptions -> IO ()
gitReadTree ReadTreeOptions{..} = runWithFoundRepo $ do
  tree <- findAndCoerceToTree optTree

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
