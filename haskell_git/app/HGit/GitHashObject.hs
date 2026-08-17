{-# LANGUAGE RecordWildCards #-}

module HGit.GitHashObject (HashObjectOptions (..), gitHashObject) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import HGit.Object
import HGit.Repository
import Relude

data HashObjectOptions = HashObjectOptions {optPath :: !FilePath, optWrite :: !Bool}

gitHashObject :: HashObjectOptions -> IO ()
gitHashObject HashObjectOptions{..} = runWithFoundRepo $ do
  contents <- readFileLBS optPath
  let obj = makeObject contents BlobObj
  when optWrite $ writeObj obj
  putStrLn $ show $ objHash obj
