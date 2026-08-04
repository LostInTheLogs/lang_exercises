{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.FastAnagrams where

import Course.Core
import Course.Functor
import Course.List
import qualified Data.Set as S
import qualified Prelude as P

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams ::
  Chars ->
  FilePath ->
  IO (List Chars)
fastAnagrams name f = do
  contents <- readFile f
  let dict = S.fromList $ NoCaseString P.<$> hlist (lines contents)
  let perms = S.fromList $ NoCaseString P.<$> hlist (permutations name)
  let r = S.intersection dict perms
  P.pure $ ncString <$> listh (S.toList r)

-- pure $ (flip (filter . flip S.member) (permutations name) . S.fromList . hlist . lines)

newtype NoCaseString
  = NoCaseString Chars

ncString ::
  NoCaseString ->
  Chars
ncString (NoCaseString s) =
  s

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  compare (NoCaseString a) (NoCaseString b) = compare (toLower <$> a) (toLower <$> b)
