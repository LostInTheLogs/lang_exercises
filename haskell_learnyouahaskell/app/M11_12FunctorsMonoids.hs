class MFunctor f where
  mfmap :: (a -> b) -> f a -> f b
  (.<$) :: a -> f b -> f a

a = fmap (++ "!") (Just "wisdom")

-- fmap (++ "!") getLine

-- `(->) r` is `r ->` so a partially applied ->
-- `->` is a type constructor too [:optimizer:](https://cdn.discordapp.com/emojis/1522362343512866959.webp?size=96)

-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))
--
-- instance Functor ((->) r) where
--     fmap = (.)

class (MFunctor f) => MApplicative f where
  mpure :: a -> f a
  (.<*>) :: f (a -> b) -> f a -> f b
  (.<*>) = mliftA2 id
  mliftA2 :: (a -> b -> c) -> f a -> f b -> f c
  mliftA2 f x = (.<*>) (mfmap f x)

b = Just (+ 3) <*> Just 3

bb = (*) <$> [1, 2, 3] <*> [10, 100, 1000]

-- [10,100,1000,20,200,2000,30,300,3000]

class (MApplicative m) => MMonad m where
  mreturn :: a -> m a
  mreturn = mpure

  (.>>=) :: m a -> (a -> m b) -> m b

  (.>>) :: m a -> m b -> m b
  x .>> y = x .>>= \_ -> y

-- Just "3!"
c = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

foo :: Maybe String
-- foo = Just 3   >>= (\x -> -
--       Just "!" >>= (\y ->
--       Just (show x ++ y)))

foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

cc = [3, 4, 5] >>= \x -> [x, -x]

-- [3,-3,4,-4,5,-5]

-- import Data.Foldable qualified as F
-- import Data.Monoid (Any (..))
--
-- data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
-- instance F.Foldable Tree where
--   foldMap f EmptyTree = mempty
--   foldMap f (Node x l r) =
--     F.foldMap f l
--       <> f x
--       <> F.foldMap f r
--
-- a = F.foldl (+) 0 testTree
-- b = getAny $ F.foldMap (\x -> Any $ x == 3) testTree
