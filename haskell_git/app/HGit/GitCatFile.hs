{-# LANGUAGE RecordWildCards #-}

module HGit.GitCatFile (CatFileOptions (..), gitCatFile) where

import qualified Data.ByteString.Lazy as BSL
import HGit.Object
import HGit.ObjectCoerce (coerceObjTo, findAndCoerceObj)
import HGit.Repository (getRepo)
import HGit.Utils (putStrErrLn)

data CatFileOptions = CatFileOptions {optType :: ObjType, optObject :: String}

gitCatFile :: CatFileOptions -> IO ()
gitCatFile CatFileOptions{..} = do
  repo <- getRepo
  obj <- findAndCoerceObj repo optType optObject
  catObject obj

catObject :: Object -> IO ()
catObject Object{objType = _, ..} = do
  BSL.putStr objPayload
