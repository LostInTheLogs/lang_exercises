module HGitLib.GitInit where

-- data AddOptions = AddOptions {optFile :: String}
data InitOptions = InitOptions {}

-- runAdd :: AddOptions -> IO ()
-- runAdd AddOptions{..} = putStrLn $ "Adding file: " ++ optFile

gitInit :: InitOptions -> IO ()
gitInit InitOptions{} = putStrLn "Initializing repo..."
