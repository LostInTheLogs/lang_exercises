{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.State where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Data.Set
import qualified Data.Set as S
import qualified Prelude as P

{- $setup
>>> import Prelude (String)
>>> import Test.QuickCheck.Function
>>> import Data.List(nub)
>>> import Test.QuickCheck
>>> import qualified Prelude as P(fmap)
>>> import Course.Core
>>> import Course.List
>>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary
-}

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a
  = State
      ( s ->
        (a, s)
      )

runState ::
  State s a ->
  s ->
  (a, s)
runState (State f) =
  f

{- | Run the `State` seeded with `s` and retrieve the resulting state.

prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
+++ OK, passed 100 tests.
-}
exec ::
  State s a ->
  s ->
  s
exec (State f) s = snd $ f s

{- | Run the `State` seeded with `s` and retrieve the resulting value.

prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
+++ OK, passed 100 tests.
-}
eval ::
  State s a ->
  s ->
  a
eval (State f) s = fst $ f s

{- | A `State` where the state also distributes into the produced value.

>>> runState get 0
(0,0)
-}
get ::
  State s s
get = State (\a -> (a, a))

{- | A `State` where the resulting state is seeded with the given value.

>>> runState (put 1) 0
((),1)
-}
put ::
  s ->
  State s ()
put a = State (const ((), a))

{- | Implement the `Functor` instance for `State s`.

>>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
(10,6)
-}
instance Functor (State s) where
  (<$>) ::
    (a -> b) ->
    State s a ->
    State s b
  f <$> (State sf) = State $ first f . sf

{- | Implement the `Applicative` instance for `State s`.

>>> runState (pure 2) 0
(2,0)

>>> runState (pure (+1) <*> pure 0) 0
(1,0)

>>> runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
(10,["apple","banana"])
-}
instance Applicative (State s) where
  pure ::
    a ->
    State s a
  pure a = State ((,) a)

  (<*>) ::
    State s (a -> b) ->
    State s a ->
    State s b
  (State sf) <*> (State sa) = State $ \s0 -> let (f, s1) = sf s0; (a, s2) = sa s1 in (f a, s2)

{- | Implement the `Monad` instance for `State s`.

>>> runState ((const $ put 2) =<< put 1) 0
((),2)

>>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
((),16)

>>> runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
(10,16)
-}
instance Monad (State s) where
  (=<<) ::
    (a -> State s b) ->
    State s a ->
    State s b
  f =<< (State sa) = State $ \s0 ->
    let (a, s1) = sa s0
        (State sb) = f a
     in sb s1

{- | Find the first element in a `List` that satisfies a given predicate.
It is possible that no element is found, hence an `Optional` result.
However, while performing the search, we sequence some `Monad` effect through.

Note the similarity of the type signature to List#find
where the effect appears in every return position:
  find ::  (a ->   Bool) -> List a ->    Optional a
  findM :: (a -> f Bool) -> List a -> f (Optional a)

>>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
(Full 'c',3)

>>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
(Empty,8)
-}
findM ::
  (Monad f) =>
  (a -> f Bool) ->
  List a ->
  f (Optional a)
findM _ Nil = pure Empty
findM predicate (h :. t) = do
  found <- predicate h
  if found then pure $ Full h else findM predicate t

{- | Find the first element in a `List` that repeats.
It is possible that no element repeats, hence an `Optional` result.

/Tip:/ Use `findM` and `State` with a @Data.Set#Set@.

>>> firstRepeat $ 1 :. 2 :. 0 :. 9 :. 2 :. 1 :. Nil
Full 2

prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (Course.List.filter (== x) xs) > 1
+++ OK, passed 100 tests.
prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
+++ OK, passed 100 tests.
-}
firstRepeat ::
  (Ord a) =>
  List a ->
  Optional a
firstRepeat list =
  let p :: (Ord a) => a -> State (Set a) Bool
      p x = do
        s <- get
        if x `member` s
          then pure True
          else do
            put $ Data.Set.insert x s
            pure False
   in eval (findM p list) Data.Set.empty

{- | Remove all duplicate elements in a `List`.
/Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.

prop> \xs -> firstRepeat (distinct xs) == Empty
+++ OK, passed 100 tests.

prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
+++ OK, passed 100 tests.
-}
distinct ::
  (Ord a) =>
  List a ->
  List a
distinct list =
  let p :: (Ord a) => a -> State (Set a) Bool
      p x = do
        s <- get
        if x `member` s
          then pure False
          else do
            put $ Data.Set.insert x s
            pure True
   in eval (filtering p list) Data.Set.empty

{- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
because it results in a recurring sequence.

/Tip:/ Use `firstRepeat` with `produce`.

/Tip:/ Use `join` to write a @square@ function.

/Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.

>>> isHappy 4
False

>>> isHappy 7
True

>>> isHappy 42
False

>>> isHappy 44
True
-}
isHappy ::
  Integer ->
  Bool
isHappy number =
  let square = join (*)
      digits :: Int -> List Int
      digits = Course.List.map digitToInt . listh . show
      sumSquareDigits :: Int -> Int
      sumSquareDigits = sum . fmap square . digits
      iterations = produce sumSquareDigits (fromInteger number)
   in contains 1 (firstRepeat iterations)
