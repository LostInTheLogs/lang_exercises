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
  obj <- readObj repo optObject
  either putStrErrLn catObject obj

catObject :: Object -> IO ()
catObject Object{objType = Blob, ..} = do
  BSL.putStr objPayload
