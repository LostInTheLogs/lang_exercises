{-# LANGUAGE RecordWildCards #-}

module HGit.GitCatFile (CatFileOptions (..), gitCatFile) where

import qualified Data.ByteString.Lazy as BSL
import HGit.FindObject (coerceObjTo, findAndCoerceObj)
import HGit.Object
import HGit.Repository (runWithFoundRepo)
import HGit.Utils (putStrErrLn)
import Relude

data CatFileOptions = CatFileOptions {optType :: ObjType, optObject :: Text}

gitCatFile :: CatFileOptions -> IO ()
gitCatFile CatFileOptions{..} = runWithFoundRepo $ do
  obj <- findAndCoerceObj optType optObject
  catObject obj

catObject :: (MonadIO m) => Object -> m ()
catObject Object{objType = _, ..} = liftIO $ do
  BSL.putStr objPayload
