{-# LANGUAGE RecordWildCards #-}

module HGit.GitAdd (gitAdd) where

data AddOptions = AddOptions {optFile :: String}

gitAdd :: AddOptions -> IO ()
gitAdd AddOptions{..} = putStrLn $ "Adding file: " ++ optFile
