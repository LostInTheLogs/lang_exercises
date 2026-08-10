{-# LANGUAGE RecordWildCards #-}

module HGit.GitCatFile (CatFileOptions (..), gitCatFile) where

import qualified Data.ByteString.Lazy as BSL
import HGit.Object
import HGit.Repository (getRepo)
import HGit.Utils (putStrErrLn)

data CatFileOptions = CatFileOptions {optType :: ObjType, optObject :: String}

gitCatFile :: CatFileOptions -> IO ()
gitCatFile CatFileOptions{..} = do
  repo <- getRepo
  objHash <- findObject repo optObject
  obj <- readObj repo optType objHash
  catObject obj

catObject :: Object -> IO ()
catObject Object{objType = _, ..} = do
  BSL.putStr objPayload
