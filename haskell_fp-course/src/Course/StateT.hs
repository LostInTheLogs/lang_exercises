{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateT where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

{- $setup
>>> import Test.QuickCheck
>>> import qualified Prelude as P(fmap)
>>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary
-}

-- | A `StateT` is a function from a state value `s` to a functor k of (a produced value `a`, and a resulting state `s`).
newtype StateT s k a
  = StateT
      ( s ->
        k (a, s)
      )

runStateT ::
  StateT s k a ->
  s ->
  k (a, s)
runStateT (StateT f) =
  f

{- | Implement the `Functor` instance for @StateT s k@ given a @Functor k@.

>>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
[(3,0)]
-}
instance (Functor k) => Functor (StateT s k) where
  (<$>) ::
    (a -> b) ->
    StateT s k a ->
    StateT s k b
  f <$> StateT ska = StateT $ fmap (first f) . ska

{- | Implement the `Applicative` instance for @StateT s k@ given a @Monad k@.

>>> runStateT (pure 2) 0
(2,0)

>>> runStateT ((pure 2) :: StateT Int List Int) 0
[(2,0)]

>>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
[(4,0)]

>>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) <*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
Full (4,[0,1,2])

>>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) <*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
[(4,[0,1,2]),(5,[0,1,2])]
-}
instance (Monad k) => Applicative (StateT s k) where
  pure ::
    a ->
    StateT s k a
  -- pure = StateT . ((pure .) .) (,)
  -- pure a = StateT $ \s -> pure (a, s)
  pure a = StateT $ pure . (a,)

  (<*>) ::
    StateT s k (a -> b) ->
    StateT s k a ->
    StateT s k b
  (StateT skf) <*> (StateT ska) = StateT
    $ \s0 -> do
      (f, s1) <- skf s0
      (a, s2) <- ska s1
      pure (f a, s2)

{- | Implement the `Monad` instance for @StateT s k@ given a @Monad k@.
Make sure the state value is passed through in `bind`.

>>> runStateT ((const $ putT 2) =<< putT 1) 0

>>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
((),16)
-}
instance (Monad k) => Monad (StateT s k) where
  (=<<) ::
    (a -> StateT s k b) ->
    StateT s k a ->
    StateT s k b
  fskb =<< (StateT ska) = StateT
    $ \s0 -> do
      (a, s1) <- ska s0
      let (StateT skb) = fskb a
      (b, s2) <- skb s1
      pure (b, s2)

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

{- | Provide a constructor for `State'` values

>>> runStateT (state' $ runState $ put 1) 0
ExactlyOne ((),1)
-}
state' ::
  (s -> (a, s)) ->
  State' s a
state' = StateT . (ExactlyOne .)

-- state' s = StateT $ (ExactlyOne . s)

{- | Provide an unwrapper for `State'` values.

>>> runState' (state' $ runState $ put 1) 0
((),1)
-}
runState' ::
  State' s a ->
  s ->
  (a, s)
runState' statet = runExactlyOne . runStateT statet

-- runState' statet state = runExactlyOne $ runStateT statet state

{- | Run the `StateT` seeded with `s` and retrieve the resulting state.

>>> execT (StateT $ \s -> Full ((), s + 1)) 2
Full 3
-}
execT ::
  (Functor k) =>
  StateT s k a ->
  s ->
  k s
execT st s = snd <$> runStateT st s

{- | Run the `State'` seeded with `s` and retrieve the resulting state.

>>> exec' (state' $ \s -> ((), s + 1)) 2
3
-}
exec' ::
  State' s a ->
  s ->
  s
exec' st s = runExactlyOne $ execT st s

{- | Run the `StateT` seeded with `s` and retrieve the resulting value.

>>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
Full True
-}
evalT ::
  (Functor k) =>
  StateT s k a ->
  s ->
  k a
evalT st s = fst <$> runStateT st s

{- | Run the `State'` seeded with `s` and retrieve the resulting value.

>>> eval' (state' $ \s -> (even s, s + 1)) 5
False
-}
eval' ::
  State' s a ->
  s ->
  a
eval' st s = runExactlyOne $ evalT st s

{- | A `StateT` where the state also distributes into the produced value.

>>> (runStateT (getT :: StateT Int List Int) 3)
[(3,3)]
-}
getT ::
  (Applicative k) =>
  StateT s k s
-- getT = StateT $ \s -> pure (s, s)
getT = StateT $ pure . join (,)

{- | A `StateT` where the resulting state is seeded with the given value.

>>> runStateT (putT 2) 0
((),2)

>>> runStateT (putT 2 :: StateT Int List ()) 0
[((),2)]
-}
putT ::
  (Applicative k) =>
  s ->
  StateT s k ()
-- putT s = StateT $ const $ pure ((), s)
putT = StateT . const . pure . ((),)

{- | Remove all duplicate elements in a `List`.

/Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.

prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
-}
distinct' ::
  (Ord a) =>
  List a ->
  List a
distinct' list = runExactlyOne $ evalT resultState S.empty
 where
  resultState = filtering filterFun list
  filterFun x = do
    s <- getT
    if x `S.member` s
      then pure False
      else do
        putT $ S.insert x s
        pure True

{- | Remove all duplicate elements in a `List`.
However, if you see a value greater than `100` in the list,
abort the computation by producing `Empty`.

/Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.

>>> distinctF $ listh [1,2,3,2,1]
Full [1,2,3]

>>> distinctF $ listh [1,2,3,2,1,101]
Empty
-}
distinctF ::
  (Ord a, Num a) =>
  List a ->
  Optional (List a)
distinctF list = evalT resultState S.empty
 where
  resultState = filtering filterFun list
  filterFun x
    | x > 100 = StateT $ const Empty
    | otherwise = do
        s <- getT
        if x `S.member` s
          then pure False
          else putT (S.insert x s) >> pure True

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT k a
  = OptionalT
  { runOptionalT ::
      k (Optional a)
  }

{- | Implement the `Functor` instance for `OptionalT k` given a Functor k.

>>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
[Full 2,Empty]
-}
instance (Functor k) => Functor (OptionalT k) where
  (<$>) ::
    (a -> b) ->
    OptionalT k a ->
    OptionalT k b
  f <$> (OptionalT ka) = OptionalT $ (f <$>) <$> ka

{- | Implement the `Applicative` instance for `OptionalT k` given a Monad k.

/Tip:/ Use `onFull` to help implement (<*>).

>>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
[]

>>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
[]

>>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
[Empty]

>>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
[Empty,Empty]

>>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
[Empty]

>>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
[Full 2,Full 3,Empty]

>>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
[Full 2,Empty,Full 3,Empty]
-}
instance (Monad k) => Applicative (OptionalT k) where
  pure ::
    a ->
    OptionalT k a
  pure = OptionalT . pure . pure

  (<*>) ::
    OptionalT k (a -> b) ->
    OptionalT k a ->
    OptionalT k b
  (OptionalT kof) <*> (OptionalT koa) = OptionalT $ do
    ofun <- kof
    optional (\f -> (f <$>) <$> koa) (pure Empty) ofun

{- | Implement the `Monad` instance for `OptionalT k` given a Monad k.

>>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
[Full 2,Full 3,Empty]
-}
instance (Monad k) => Monad (OptionalT k) where
  (=<<) ::
    (a -> OptionalT k b) ->
    OptionalT k a ->
    OptionalT k b
  f =<< (OptionalT koa) = OptionalT $ do
    oa <- koa
    optional (runOptionalT . f) (pure Empty) oa

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a
  = Logger (List l) a
  deriving (Eq, Show)

{- | Implement the `Functor` instance for `Logger

>>> (+3) <$> Logger (listh [1,2]) 3
Logger [1,2] 6
-}
instance Functor (Logger l) where
  (<$>) ::
    (a -> b) ->
    Logger l a ->
    Logger l b
  f <$> (Logger logs a) = Logger logs (f a)

{- | Implement the `Applicative` instance for `Logger`.

>>> pure "table" :: Logger Int P.String
Logger [] "table"

>>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
Logger [1,2,3,4] 10
-}
instance Applicative (Logger l) where
  pure ::
    a ->
    Logger l a
  pure = Logger Nil

  (<*>) ::
    Logger l (a -> b) ->
    Logger l a ->
    Logger l b
  (Logger fl f) <*> (Logger al a) = Logger (fl ++ al) (f a)

{- | Implement the `Monad` instance for `Logger`.
The `bind` implementation must append log values to maintain associativity.

>>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
Logger [1,2,4,5] 6
-}
instance Monad (Logger l) where
  (=<<) ::
    (a -> Logger l b) ->
    Logger l a ->
    Logger l b
  f =<< (Logger al a) =
    let (Logger bl b) = f a
     in Logger (al ++ bl) b

{- | A utility function for producing a `Logger` with one log value.

>>> log1 1 2
Logger [1] 2
-}
log1 ::
  l ->
  a ->
  Logger l a
log1 = Logger . pure

tell :: List l -> Logger l ()
tell a = Logger a ()

tell1 :: l -> Logger l ()
tell1 a = Logger (a :. Nil) ()

{- | Remove all duplicate integers from a list. Produce a log as you go.
If there is an element above 100, then abort the entire computation and produce no result.
However, always keep a log. If you abort the computation, produce a log with the value,
"aborting > 100: " followed by the value that caused it.
If you see an even number, produce a log message, "even number: " followed by the even number.
Other numbers produce no log message.

>>> distinctG $ listh [1,2,3,2,6]
Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])

>>> distinctG $ listh [1,2,3,2,6,106]
Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
-}
distinctG ::
  (Integral a, Show a) =>
  List a ->
  Logger Chars (Optional (List a))
distinctG list = runOptionalT $ evalT (filtering filterFun list) S.empty
 where
  filterFun ::
    (Integral a, Show a) =>
    a ->
    StateT (S.Set a) (OptionalT (Logger Chars)) Bool
  filterFun a = StateT $ \s ->
    OptionalT
      $ if a > 100
        then
          log1 ("aborting > 100: " ++ listh (show a)) Empty
        else do
          when (even a) $ tell1 $ "even number: " ++ listh (show a)
          pure $ Full (a `S.notMember` s, S.insert a s)

when :: (Monad m) => Bool -> m () -> m ()
when True p = p
when False _ = pure ()

onFull ::
  (Applicative k) =>
  (t -> k (Optional a)) ->
  Optional t ->
  k (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
