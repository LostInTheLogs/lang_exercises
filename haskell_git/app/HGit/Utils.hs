module HGit.Utils (putStrErrLn, note) where

putStrErrLn :: [Char] -> IO ()
putStrErrLn err = putStrLn $ "Error: " ++ err

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
