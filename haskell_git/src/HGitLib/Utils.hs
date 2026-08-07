module HGitLib.Utils (putStrErrLn) where

putStrErrLn :: [Char] -> IO ()
putStrErrLn err = putStrLn $ "Error: " ++ err
