{-# LANGUAGE RecordWildCards #-}

module HGit.GitLog (gitLog, LogOptions (..)) where

import HGit.Commit (parseCommit)
import HGit.Object (ObjType (Commit), findObject, hashToStr, objPayload, readObj, strToHash)
import HGit.Repository (getRepo)

data LogOptions = LogOptions {optRef :: String}

gitLog :: LogOptions -> IO ()
gitLog LogOptions{..} = do
  putStrLn "log"
  repo <- getRepo
  obj <- readObj repo Commit =<< findObject repo optRef
  putStrLn $ show $ parseCommit $ objPayload obj
