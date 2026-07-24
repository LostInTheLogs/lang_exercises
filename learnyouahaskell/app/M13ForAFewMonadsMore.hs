import Control.Monad.Writer
import Data.Ratio

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- here the list appends are fast: a ++ (b ++ (c ++ (d ++ (e ++ f))))

-- 8 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- Finished with 1
a = mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

-- here they're slow: ((((a ++ b) ++ c) ++ d) ++ e) ++ f

-- Finished with 1
-- 2 mod 1 = 0
-- 3 mod 2 = 1
-- 8 mod 3 = 2
b = mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where
    multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
    pure x = Prob [(x, 1 % 1)]

instance Monad Prob where
    return = pure
    m >>= f = flatten (fmap f m)

instance MonadFail Prob where
    fail _ = Prob []
