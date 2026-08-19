module HGit.Utils (
  putStrErrLn,
  note,
  fReadTxtLine,
  fReadStrLine,
  fReadBSLine,
  runParserUnsafe,
  runParserUnsafe2,
  throwErr,
  throwStrErr,
  binarySearch,
  insertManySorted,
  nameParser,
) where

import Control.Monad.ST (runST)
import Data.Attoparsec.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import Data.List.Extra (headDef)
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Algorithms.Heap as VS
import qualified Data.Vector.Algorithms.Search as VAS
import qualified Data.Vector.Strict as V
import Relude
import qualified Relude.Unsafe as Unsafe
import qualified System.IO as IO
import qualified Prelude as P (error)

putStrErrLn :: [Char] -> IO ()
putStrErrLn err = putStrLn $ "Error: " ++ err

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

fReadTxtLine :: (MonadIO m) => FilePath -> m Text
fReadTxtLine path = liftIO $ withFile path ReadMode TIO.hGetLine

fReadStrLine :: (MonadIO m) => FilePath -> m String
fReadStrLine path = liftIO $ withFile path ReadMode IO.hGetLine

fReadBSLine :: (MonadIO m) => FilePath -> m ByteString
fReadBSLine path = liftIO $ withFile path ReadMode BSC8.hGetLine

runParserUnsafe :: A.Parser a -> BSL.ByteString -> a
runParserUnsafe parser input = do
  let res = A.parse parser input
  case A.eitherResult res of
    Right obj -> obj
    Left err -> throwStrErr "runParserUnsafe" err

runParserUnsafe2 :: A.Parser a -> BSL.ByteString -> (a, BSL.ByteString)
runParserUnsafe2 parser input = do
  let res = A.parse parser input
  case res of
    A.Done rest obj -> (obj, rest)
    A.Fail _ [] msg -> throwStrErr "runParserUnsafe2" msg
    A.Fail _ ctx msg -> throwStrErr "runParserUnsafe2" (intercalate " > " ctx <> ": " <> msg)

throwErr :: Text -> Text -> a
throwErr who msg = error $ "fatal: " <> who <> ": " <> msg

throwStrErr :: String -> String -> a
throwStrErr who msg = P.error $ "fatal: " <> who <> ": " <> msg

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

insertManySorted :: (Ord a) => V.Vector a -> V.Vector a -> V.Vector a
insertManySorted large small = V.modify VS.sort (large V.++ small)

nameParser :: String -> A.Parser a -> A.Parser a
nameParser name parser = parser <?> name
