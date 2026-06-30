lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

a = do
  let xs = [(1, 3), (4, 3), (2, 4), (5, 3), (5, 6), (3, 1)]
  [a + b | (a, b) <- xs]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

length' :: (Num a) => [b] -> a
length' [] = 0
length' (_ : t) = 1 + length' t

max_ :: (Ord a) => a -> a -> a
max_ a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
  | density < 1.2 = "Wow! You're going for a ride in the sky!"
  | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
  | otherwise = "If it's sink or swim, you're going to sink."
 where
  density = mass / volume
