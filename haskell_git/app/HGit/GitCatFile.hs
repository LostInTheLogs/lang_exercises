{-# LANGUAGE RecordWildCards #-}

module HGit.GitCatFile (CatFileOptions (..), gitCatFile) where

import HGit.Object

data CatFileOptions = CatFileOptions {optType :: ObjType, optObject :: String}

gitCatFile :: CatFileOptions -> IO ()
gitCatFile CatFileOptions{..} = do
  _
