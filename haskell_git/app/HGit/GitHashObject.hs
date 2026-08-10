{-# LANGUAGE RecordWildCards #-}

module HGit.GitHashObject (HashObjectOptions (..), gitHashObject) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import HGit.Object
import HGit.Repository

data HashObjectOptions = HashObjectOptions {optPath :: !FilePath, optWrite :: !Bool}

gitHashObject :: HashObjectOptions -> IO ()
gitHashObject HashObjectOptions{..} = do
  contents <- BSL.readFile optPath
  let obj = makeObject contents Blob
  repo <- getRepo
  when optWrite $ writeObj repo obj
  putStrLn $ hashToStr $ objHash obj
