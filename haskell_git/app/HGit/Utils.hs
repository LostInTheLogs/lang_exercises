module HGit.Utils (
  putStrErrLn,
  note,
  fReadLine,
  fReadBSLine,
  runParserUnsafe,
  throwErr,
) where

import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
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

runParserUnsafe :: A.Parser a -> BSL.ByteString -> a
runParserUnsafe parser input = do
  let res = A.parse parser input
  case A.eitherResult res of
    Right obj -> obj
    Left err -> throwErr "runParserUnsafe" err

throwErr :: String -> String -> a
throwErr who msg = error $ "fatal: " <> who <> ": " <> msg
