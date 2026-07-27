maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty lists dont have a maximum"
maximum' [x] = x
maximum' (h : rest)
  | h > rest_max = h
  | otherwise = rest_max
 where
  rest_max = maximum' rest

-- repeat' :: a -> Int -> [a]
-- repeat' _ 0 = []
-- repeat' item times = item : (repeat' item (times - 1))

repeat' :: (Integral b) => a -> b -> [a]
repeat' x n
  | n <= 0 = []
  | otherwise = x : (repeat' x (n - 1))

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a : aRest) (b : bRest) = (a, b) : (zip' aRest bRest)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' needle (x : xs)
  | needle == x = True
  | otherwise = elem' needle xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let
    smaller = quicksort [a | a <- xs, a <= x]
    bigger = quicksort [a | a <- xs, a > x]
   in
    smaller ++ [x] ++ bigger
