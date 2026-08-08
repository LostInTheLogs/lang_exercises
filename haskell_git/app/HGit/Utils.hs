module HGit.Utils (putStrErrLn) where

putStrErrLn :: [Char] -> IO ()
putStrErrLn err = putStrLn $ "Error: " ++ err
