module HGit.Utils (
  putStrErrLn,
  note,
  fReadLine,
  fReadBSLine,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import System.IO (IOMode (ReadMode), hGetLine, withFile)

putStrErrLn :: [Char] -> IO ()
putStrErrLn err = putStrLn $ "Error: " ++ err

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

fReadLine :: FilePath -> IO String
fReadLine path = withFile path ReadMode hGetLine

fReadBSLine :: FilePath -> IO BS.ByteString
fReadBSLine path = withFile path ReadMode BSC8.hGetLine
