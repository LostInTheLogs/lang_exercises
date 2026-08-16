module HGit.Utils (
  putStrErrLn,
  note,
  fReadLine,
  fReadBSLine,
  runParserUnsafe,
  runParserUnsafe2,
  throwErr,
  binarySearch,
  nameParser,
  mapMaybeM,
) where

import Data.Attoparsec.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
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

runParserUnsafe2 :: A.Parser a -> BSL.ByteString -> (a, BSL.ByteString)
runParserUnsafe2 parser input = do
  let res = A.parse parser input
  case res of
    A.Done rest obj -> (obj, rest)
    A.Fail _ [] msg -> throwErr "runParserUnsafe2" msg
    A.Fail _ ctx msg -> throwErr "runParserUnsafe2" (List.intercalate " > " ctx ++ ": " ++ msg)

throwErr :: String -> String -> a
throwErr who msg = error $ "fatal: " <> who <> ": " <> msg

{- | Performs a binary search on a sorted Vector.
Returns `Just index` if found, or `Nothing` if the target doesn't exist.
-}
binarySearch :: (Ord a) => V.Vector a -> a -> Maybe Int
binarySearch vec target = loop 0 (V.length vec - 1)
 where
  loop low high
    | low > high = Nothing
    | otherwise =
        let mid = low + (high - low) `div` 2
            val = vec V.! mid
         in case compare target val of
              LT -> loop low (mid - 1)
              GT -> loop (mid + 1) high
              EQ -> Just mid

nameParser :: String -> A.Parser a -> A.Parser a
nameParser name parser = parser <?> name

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f
