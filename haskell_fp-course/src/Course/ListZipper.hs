{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipper where

import Course.Applicative
import Course.Comonad
import Course.Core
import Course.Extend
import Course.Functor
import Course.List
import Course.Optional
import Course.Traversable
import qualified Prelude as P

import Control.Monad (join)
import qualified Data.Bifunctor as DB
import Prelude (String, (>>=))

{- $setup
>>> import Test.QuickCheck
>>> import Data.Maybe(maybe)
>>> import Course.Core
>>> import qualified Prelude as P
>>> let optional e _ Empty = e; optional _ f (Full a) = f a
>>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
>>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
>>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)
-}

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a
  = ListZipper (List a) a (List a)
  deriving (Eq)

lefts ::
  ListZipper a ->
  List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a ->
  List a
rights (ListZipper _ _ r) =
  r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
newtype MaybeListZipper a
  = MLZ (Optional (ListZipper a))
  deriving (Eq)

isZ ::
  ListZipper a ->
  MaybeListZipper a
isZ = MLZ . Full

isNotZ ::
  MaybeListZipper a
isNotZ = MLZ Empty

{- | Implement the `Functor` instance for `ListZipper`.

>>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
[4,3,2] >5< [6,7,8]
-}
instance Functor ListZipper where
  (<$>) :: (a -> b) -> ListZipper a -> ListZipper b
  f <$> (ListZipper l c r) = ListZipper (f <$> l) (f c) (f <$> r)

{- | Implement the `Functor` instance for `MaybeListZipper`.

>>> (+1) <$> (MLZ (Full (zipper [3,2,1] 4 [5,6,7])))
[4,3,2] >5< [6,7,8]
-}
instance Functor MaybeListZipper where
  (<$>) :: (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  _ <$> MLZ Empty = MLZ Empty
  f <$> MLZ (Full z) = MLZ $ Full $ f <$> z

{- | Convert the given zipper back to a list.

>>> toList <$> toOptional (fromList Nil)
Empty

>>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
[1,2,3,4]

>>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
[1,2,3,4,5,6,7]
-}
toList ::
  ListZipper a ->
  List a
toList (ListZipper l c r) = reverse l ++ c :. r

-- | Convert the given (maybe) zipper back to a list.
toListZ ::
  MaybeListZipper a ->
  List a
toListZ (MLZ Empty) =
  Nil
toListZ (MLZ (Full z)) =
  toList z

{- | Create a `MaybeListZipper` positioning the focus at the head.

>>> fromList (1 :. 2 :. 3 :. Nil)
[] >1< [2,3]

>>> fromList Nil
><

prop> \xs -> xs == toListZ (fromList xs)
+++ OK, passed 100 tests.
-}
fromList ::
  List a ->
  MaybeListZipper a
fromList (h :. r) = MLZ $ Full $ ListZipper Nil h r
fromList Nil = MLZ Empty

{- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.

prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
+++ OK, passed 100 tests.

prop> \z -> toOptional (fromOptional z) == z
+++ OK, passed 100 tests.
-}
toOptional ::
  MaybeListZipper a ->
  Optional (ListZipper a)
toOptional (MLZ Empty) = Empty
toOptional (MLZ (Full a)) = Full a

zipper ::
  [a] ->
  a ->
  [a] ->
  ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional ::
  Optional (ListZipper a) ->
  MaybeListZipper a
fromOptional =
  MLZ

asZipper ::
  (ListZipper a -> ListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
asZipper f =
  asMaybeZipper (isZ . f)

(>$>) ::
  (ListZipper a -> ListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
asMaybeZipper _ (MLZ Empty) =
  isNotZ
asMaybeZipper f (MLZ (Full z)) =
  f z

(-<<) ::
  (ListZipper a -> MaybeListZipper a) ->
  MaybeListZipper a ->
  MaybeListZipper a
(-<<) =
  asMaybeZipper

{- | Update the focus of the zipper with the given function on the current focus.

>>> withFocus (+1) (zipper [] 0 [1])
[] >1< [1]

>>> withFocus (+1) (zipper [1,0] 2 [3,4])
[1,0] >3< [3,4]
-}
withFocus ::
  (a -> a) ->
  ListZipper a ->
  ListZipper a
withFocus f (ListZipper l c r) = ListZipper l (f c) r

{- | Set the focus of the zipper to the given value.
/Tip:/ Use `withFocus`.

>>> setFocus 1 (zipper [] 0 [1])
[] >1< [1]

>>> setFocus 1 (zipper [1,0] 2 [3,4])
[1,0] >1< [3,4]
-}
setFocus ::
  a ->
  ListZipper a ->
  ListZipper a
setFocus a = withFocus (const a)

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper a ->
  a ->
  ListZipper a
(.=) =
  flip setFocus

{- | Returns whether there are values to the left of focus.

>>> hasLeft (zipper [1,0] 2 [3,4])
True

>>> hasLeft (zipper [] 0 [1,2])
False
-}
hasLeft ::
  ListZipper a ->
  Bool
hasLeft (ListZipper Nil _ _) = False
hasLeft _ = True

{- | Returns whether there are values to the right of focus.

>>> hasRight (zipper [1,0] 2 [3,4])
True

>>> hasRight (zipper [1,0] 2 [])
False
-}
hasRight ::
  ListZipper a ->
  Bool
hasRight (ListZipper _ _ Nil) = False
hasRight _ = True

{- | Seek to the left for a location matching a predicate, excluding the
focus.

/Tip:/ Use `break`

prop> \xs p -> findLeft (const p) -<< fromList xs == isNotZ
+++ OK, passed 100 tests.

>>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
[] >1< [2,3,4,5]

>>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
><

>>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
[] >1< [2,1,4,5]

>>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
[2,1] >1< [3,4,5]

>>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
[5] >1< [4,3,9,2,7]
-}
findLeft ::
  (a -> Bool) ->
  ListZipper a ->
  MaybeListZipper a
findLeft p (ListZipper l c r)
  | (curr :. nl) <- curr_nl = MLZ $ Full $ ListZipper nl curr (reverse centerRight ++ c :. r)
  | otherwise = MLZ Empty
 where
  (centerRight, curr_nl) = break p l

{- | Seek to the right for a location matching a predicate, excluding the
focus.

/Tip:/ Use `break`

prop> \xs -> findRight (const False) -<< fromList xs == isNotZ
+++ OK, passed 100 tests.

>>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
[4,3,2,1] >5< []

>>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
><

>>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
[5,4,1,2,3] >1< []

>>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
[1,2,3] >1< [4,5,1]
-}
findRight ::
  (a -> Bool) ->
  ListZipper a ->
  MaybeListZipper a
findRight p (ListZipper l c r)
  | (curr :. nr) <- curr_nr = MLZ $ Full $ ListZipper (reverse revCenterLeft ++ c :. l) curr nr
  | otherwise = MLZ Empty
 where
  (revCenterLeft, curr_nr) = break p r

{- | Move the zipper left, or if there are no elements to the left, go to the far right.

>>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
[2,1] >3< [4,5,6,7]

>>> moveLeftLoop (zipper [] 1 [2,3,4])
[3,2,1] >4< []
-}
moveLeftLoop ::
  ListZipper a ->
  ListZipper a
moveLeftLoop (ListZipper Nil c r) = let (nc :. nl) = reverse (c :. r) in ListZipper nl nc Nil
moveLeftLoop (ListZipper (nc :. nl) c r) = ListZipper nl nc (c :. r)

{- | Move the zipper right, or if there are no elements to the right, go to the far left.

>>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
[4,3,2,1] >5< [6,7]

>>> moveRightLoop (zipper [3,2,1] 4 [])
[] >1< [2,3,4]
-}
moveRightLoop ::
  ListZipper a ->
  ListZipper a
moveRightLoop (ListZipper l c Nil) = let (nc :. nr) = reverse (c :. l) in ListZipper Nil nc nr
moveRightLoop (ListZipper l c (nc :. nr)) = ListZipper (c :. l) nc nr

{- | Move the zipper one position to the left.

>>> moveLeft (zipper [3,2,1] 4 [5,6,7])
[2,1] >3< [4,5,6,7]

>>> moveLeft (zipper [] 1 [2,3,4])
><
-}
moveLeft ::
  ListZipper a ->
  MaybeListZipper a
moveLeft (ListZipper Nil _ _) = MLZ Empty
moveLeft (ListZipper (nc :. nl) c r) = MLZ $ Full $ ListZipper nl nc (c :. r)

{- | Move the zipper one position to the right.

>>> moveRight (zipper [3,2,1] 4 [5,6,7])
[4,3,2,1] >5< [6,7]

>>> moveRight (zipper [3,2,1] 4 [])
><
-}
moveRight ::
  ListZipper a ->
  MaybeListZipper a
moveRight (ListZipper _ _ Nil) = MLZ Empty
moveRight (ListZipper l c (nc :. nr)) = MLZ $ Full $ ListZipper (c :. l) nc nr

{- | Swap the current focus with the value to the left of focus.

>>> swapLeft (zipper [3,2,1] 4 [5,6,7])
[4,2,1] >3< [5,6,7]

>>> swapLeft (zipper [] 1 [2,3,4])
><
-}
swapLeft ::
  ListZipper a ->
  MaybeListZipper a
swapLeft (ListZipper Nil _ _) = MLZ Empty
swapLeft (ListZipper (nc :. nl) oc r) = MLZ $ Full $ ListZipper (oc :. nl) nc r

{- | Swap the current focus with the value to the right of focus.

>>> swapRight (zipper [3,2,1] 4 [5,6,7])
[3,2,1] >5< [4,6,7]

>>> swapRight (zipper [3,2,1] 4 [])
><
-}
swapRight ::
  ListZipper a ->
  MaybeListZipper a
swapRight (ListZipper _ _ Nil) = MLZ Empty
swapRight (ListZipper l oc (nc :. nr)) = MLZ $ Full $ ListZipper l nc (oc :. nr)

{- | Drop all values to the left of the focus.

>>> dropLefts (zipper [3,2,1] 4 [5,6,7])
[] >4< [5,6,7]

>>> dropLefts (zipper [] 1 [2,3,4])
[] >1< [2,3,4]

prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
+++ OK, passed 100 tests.
-}
dropLefts ::
  ListZipper a ->
  ListZipper a
dropLefts (ListZipper _ c r) = ListZipper Nil c r

{- | Drop all values to the right of the focus.

>>> dropRights (zipper [3,2,1] 4 [5,6,7])
[3,2,1] >4< []

>>> dropRights (zipper [3,2,1] 4 [])
[3,2,1] >4< []

prop> \l x r -> dropRights (zipper l x r) == zipper l x []
+++ OK, passed 100 tests.
-}
dropRights ::
  ListZipper a ->
  ListZipper a
dropRights (ListZipper l c _) = ListZipper l c Nil

{- | Move the focus left the given number of positions. If the value is negative, move right instead.

>>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
[0] >1< [2,3,4,5,6]

>>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
[3,2,1,0] >4< [5,6]
-}
moveLeftN ::
  Int ->
  ListZipper a ->
  MaybeListZipper a
moveLeftN n a
  | n < 0 = moveRightN (negate n) a
  | n == 0 = isZ a
  | otherwise = moveLeftN (n - 1) -<< moveLeft a

{- | Move the focus right the given number of positions. If the value is negative, move left instead.

>>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
[3,2,1,0] >4< [5,6]

>>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
[1,0] >2< [3,4,5,6]
-}
moveRightN ::
  Int ->
  ListZipper a ->
  MaybeListZipper a
moveRightN n a
  | n < 0 = moveLeftN (negate n) a
  | n == 0 = isZ a
  | otherwise = moveRightN (n - 1) -<< moveRight a

{- | Move the focus left the given number of positions. If the value is negative, move right instead.
If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.

>>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
Left 3

>>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
Right [2,1] >3< [4,5,6,7]

>>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
Right [3,2,1] >4< [5,6,7]

>>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
Right [5,4,3,2,1] >6< [7]

>>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
Left 3

>>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
Left 3

>>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
Left 3

>>> rights <$> moveLeftN' 1 (zipper [3,2,error "moveLeftN' not sufficiently lazy"] 4 [5,6,7])
Right [4,5,6,7]
-}
moveLeftN' ::
  Int ->
  ListZipper a ->
  Either Int (ListZipper a)
moveLeftN' n a
  | n < 0 = moveRightN' (negate n) a
  | n == 0 = Right a
  | otherwise = case moveLeft a of
      MLZ (Full moved) -> DB.first (+ 1) $ moveLeftN' (n - 1) moved
      MLZ Empty -> Left 0

{- | Move the focus right the given number of positions. If the value is negative, move left instead.
If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.

>>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
Left 3

>>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
Right [4,3,2,1] >5< [6,7]

>>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
Right [3,2,1] >4< [5,6,7]

>>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
Right [1] >2< [3,4,5,6,7]

>>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
Left 3
-}
moveRightN' ::
  Int ->
  ListZipper a ->
  Either Int (ListZipper a)
moveRightN' n a
  | n < 0 = moveLeftN' (negate n) a
  | n == 0 = Right a
  | otherwise = case moveRight a of
      MLZ (Full moved) -> DB.first (+ 1) $ moveRightN' (n - 1) moved
      MLZ Empty -> Left 0

{- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.

>>> nth 1 (zipper [3,2,1] 4 [5,6,7])
[1] >2< [3,4,5,6,7]

>>> nth 5 (zipper [3,2,1] 4 [5,6,7])
[5,4,3,2,1] >6< [7]

>>> nth 8 (zipper [3,2,1] 4 [5,6,7])
><
-}
nth ::
  Int ->
  ListZipper a ->
  MaybeListZipper a
nth n a@(ListZipper l _ _) = moveLeftN (length l - n) a

{- | Return the absolute position of the current focus in the zipper.

>>> index (zipper [3,2,1] 4 [5,6,7])
3

prop> \i z -> optional True (\z' -> index z' == i) (toOptional (nth i z))
+++ OK, passed 100 tests.
-}
index ::
  ListZipper a ->
  Int
index (ListZipper l _ _) = length l

{- | Move the focus to the end of the zipper.

>>> end (zipper [3,2,1] 4 [5,6,7])
[6,5,4,3,2,1] >7< []

prop> \lz -> toList lz == toList (end lz)
+++ OK, passed 100 tests.

prop> \lz -> rights (end lz) == Nil
+++ OK, passed 100 tests.
-}
end ::
  ListZipper a ->
  ListZipper a
end (ListZipper l c r) = let (nc :. nl) = reverse r ++ c :. l in ListZipper nl nc Nil

{- | Move the focus to the start of the zipper.

>>> start (zipper [3,2,1] 4 [5,6,7])
[] >1< [2,3,4,5,6,7]

prop> \lz -> toList lz == toList (start lz)
+++ OK, passed 100 tests.

prop> \lz -> lefts (start lz) == Nil
+++ OK, passed 100 tests.
-}
start ::
  ListZipper a ->
  ListZipper a
start (ListZipper l c r) = let (nc :. nr) = reverse l ++ c :. r in ListZipper Nil nc nr

{- | Delete the current focus and pull the left values to take the empty position.

>>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
[2,1] >3< [5,6,7]

>>> deletePullLeft (zipper [] 1 [2,3,4])
><
-}
deletePullLeft ::
  ListZipper a ->
  MaybeListZipper a
deletePullLeft a = (\(ListZipper l c (_ :. nr)) -> isZ $ ListZipper l c nr) -<< moveLeft a

{- | Delete the current focus and pull the right values to take the empty position.

>>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
[3,2,1] >5< [6,7]

>>> deletePullRight (zipper [3,2,1] 4 [])
><
-}
deletePullRight ::
  ListZipper a ->
  MaybeListZipper a
deletePullRight a = (\(ListZipper (_ :. nl) c r) -> isZ $ ListZipper nl c r) -<< moveRight a

{- | Insert at the current focus and push the left values to make way for the new position.

>>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
[4,3,2,1] >15< [5,6,7]

>>> insertPushLeft 15 (zipper [] 1 [2,3,4])
[1] >15< [2,3,4]

prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
+++ OK, passed 100 tests.
-}
insertPushLeft ::
  a ->
  ListZipper a ->
  ListZipper a
insertPushLeft a (ListZipper l c r) = ListZipper (c :. l) a r

{- | Insert at the current focus and push the right values to make way for the new position.

>>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
[3,2,1] >15< [4,5,6,7]

>>> insertPushRight 15 (zipper [3,2,1] 4 [])
[3,2,1] >15< [4]

prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
+++ OK, passed 100 tests.
-}
insertPushRight ::
  a ->
  ListZipper a ->
  ListZipper a
insertPushRight a (ListZipper l c r) = ListZipper l a (c :. r)

{- | Implement the `Applicative` instance for `ListZipper`.
`pure` produces an infinite list zipper (to both left and right).
(<*>) zips functions with values by function application.

prop> \n -> all . (==) <*> take n . lefts . Course.Applicative.pure
+++ OK, passed 100 tests.

prop> \n -> all . (==) <*> take n . rights . Course.Applicative.pure
+++ OK, passed 100 tests.

>>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
[5,12] >8< [15,24,12]
-}
instance Applicative ListZipper where
  -- /Tip:/ Use @List#repeat@.
  pure a = ListZipper (produce (const a) a) a (produce (const a) a)

  -- /Tip:/ Use `zipWith`
  (<*>) :: ListZipper (a -> b) -> ListZipper a -> ListZipper b
  (ListZipper lf cf rf) <*> (ListZipper l c r) = ListZipper (zipWith ($) lf l) (cf c) (zipWith ($) rf r)

{- | Implement the `Applicative` instance for `MaybeListZipper`.

/Tip:/ Use @pure@ for `ListZipper`.
/Tip:/ Use `<*>` for `ListZipper`.

prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . lefts . is . pure
+++ OK, passed 100 tests.

prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . rights . is . pure
+++ OK, passed 100 tests.

>>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isZ (zipper [3,2,1] 4 [5,6,7])
[5,12] >8< [15,24,12]

>>> isNotZ <*> isZ (zipper [3,2,1] 4 [5,6,7])
><

>>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isNotZ
><

>>> isNotZ <*> isNotZ
><
-}
instance Applicative MaybeListZipper where
  pure :: a -> MaybeListZipper a
  pure = isZ . pure

  (<*>) :: MaybeListZipper (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  MLZ Empty <*> _ = isNotZ
  _ <*> MLZ Empty = isNotZ
  MLZ (Full a) <*> (MLZ (Full b)) = isZ $ a <*> b

{- | Implement the `Extend` instance for `ListZipper`.
This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).

/Tip:/ Use @List#unfoldr@.

>>> id <<= (zipper [2,1] 3 [4,5])
[[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
-}
instance Extend ListZipper where
  (<<=) :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
  f <<= a =
    let lefts = unfoldr (\z -> join (,) <$> toOptional (moveLeft z)) a
        rights = unfoldr (\z -> join (,) <$> toOptional (moveRight z)) a
     in ListZipper (f <$> lefts) (f a) (f <$> rights)

-- f <<= a@(ListZipper l c r) =
--   let lefts = take (length l) $ drop 1 $ produce (\x -> let (MLZ (Full r)) = moveLeft x in r) a
--       rights = take (length r) $ drop 1 $ produce (\x -> let (MLZ (Full r)) = moveRight x in r) a
--    in ListZipper (f <$> lefts) (f a) (f <$> rights)

{- | Implement the `Extend` instance for `MaybeListZipper`.
This instance will use the `Extend` instance for `ListZipper`.

id <<= isNotZ
><

>>> id <<= (isZ (zipper [2,1] 3 [4,5]))
[[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
-}
instance Extend MaybeListZipper where
  _ <<= (MLZ Empty) = isNotZ
  a <<= (MLZ (Full b)) = isZ $ (a . isZ) <<= b

{- | Implement the `Comonad` instance for `ListZipper`.
This implementation returns the current focus of the zipper.

>>> copure (zipper [2,1] 3 [4,5])
3
-}
instance Comonad ListZipper where
  copure (ListZipper _ c _) = c

{- | Implement the `Traversable` instance for `ListZipper`.
This implementation traverses a zipper from left to right while running
some `Applicative` effect through the zipper.
An effectful zipper is returned.

>>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
Full [1,2,3] >4< [5,6,7]

>>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
Empty

>>> traverse id (zipper [error "traversing left values in wrong order", Empty] (error "traversing focus before left values") [Full 5, Full 6, Full 7])
Empty
-}
instance Traversable ListZipper where
  traverse ::
    (Applicative k) =>
    (a -> k b) ->
    ListZipper a ->
    k (ListZipper b)
  traverse f (ListZipper l c r) =
    let nl = reverse <$> traverse f (reverse l)
        nr = traverse f r
     in ListZipper <$> nl <*> f c <*> nr

{- | Implement the `Traversable` instance for `MaybeListZipper`.

/Tip:/ Use `traverse` for `ListZipper`.

>>> traverse id isNotZ
><

>>> traverse id (isZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
Full [1,2,3] >4< [5,6,7]
-}
instance Traversable MaybeListZipper where
  traverse _ (MLZ Empty) = pure isNotZ
  traverse f (MLZ (Full a)) = isZ <$> traverse f a

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance (Show a) => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance (Show a) => Show (MaybeListZipper a) where
  show (MLZ (Full z)) = show z
  show (MLZ Empty) = "><"
