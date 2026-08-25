module HGit.Utils (
  putStrErrLn,
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
  Parser,
  runFParserUnsafe,
) where

import Control.Monad.ST (runST)
import Data.Attoparsec.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import Data.List.Extra (headDef)
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as VS
import qualified Data.Vector.Algorithms.Search as VAS
import qualified FlatParse.Basic as FP
import Relude
import qualified Relude.Unsafe as Unsafe
import qualified System.IO as IO
import qualified Prelude as P (error)

putStrErrLn :: [Char] -> IO ()
putStrErrLn err = putStrLn $ "Error: " ++ err

{-# INLINE fReadTxtLine #-}
fReadTxtLine :: (MonadIO m) => FilePath -> m Text
fReadTxtLine path = liftIO $ withFile path ReadMode TIO.hGetLine

{-# INLINE fReadStrLine #-}
fReadStrLine :: (MonadIO m) => FilePath -> m String
fReadStrLine path = liftIO $ withFile path ReadMode IO.hGetLine

{-# INLINE fReadBSLine #-}
fReadBSLine :: (MonadIO m) => FilePath -> m ByteString
fReadBSLine path = liftIO $ withFile path ReadMode BSC8.hGetLine

type Parser = FP.Parser Text

runFParserUnsafe :: (HasCallStack) => Parser a -> BS.ByteString -> a
runFParserUnsafe parser input = withFrozenCallStack $ do
  let res = FP.runParser parser input
  case res of
    FP.Err e -> throwErr "runFParserUnsafe" e
    FP.OK a _ -> a
    FP.Fail -> throwStrErr "runFParserUnsafe" "uncaught parser error"

runParserUnsafe :: (HasCallStack) => A.Parser a -> BSL.ByteString -> a
runParserUnsafe parser input = withFrozenCallStack $ do
  let res = A.parse parser input
  case A.eitherResult res of
    Right obj -> obj
    Left err -> throwStrErr "runParserUnsafe" err

runParserUnsafe2 :: (HasCallStack) => A.Parser a -> BSL.ByteString -> (a, BSL.ByteString)
runParserUnsafe2 parser input = withFrozenCallStack $ do
  let res = A.parse parser input
  case res of
    A.Done rest obj -> (obj, rest)
    A.Fail _ [] msg -> throwStrErr "runParserUnsafe2" msg
    A.Fail _ ctx msg -> throwStrErr "runParserUnsafe2" (intercalate " > " ctx <> ": " <> msg)

{-# INLINE throwErr #-}
throwErr :: (HasCallStack) => Text -> Text -> a
throwErr who msg = withFrozenCallStack $ error $ "fatal: " <> who <> ": " <> msg

{-# INLINE throwStrErr #-}
throwStrErr :: (HasCallStack) => String -> String -> a
throwStrErr who msg = withFrozenCallStack $ P.error $ "fatal: " <> who <> ": " <> msg

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

{-# INLINE nameParser #-}
nameParser :: String -> A.Parser a -> A.Parser a
nameParser name parser = parser <?> name
