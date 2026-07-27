compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a : as) (b : bs) = (f a b) : (zipWith' f as bs)

a = sqrt (3 + 4 + 9)
a = sqrt $ 3 + 4 + 9

b = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
b = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

fn x = ceiling (negate (tan (cos (max 50 x))))
fn x = ceiling $ negate $ tan $ cos $ max 50 x
fn = ceiling . negate . tan . cos . max 50 -- pointless style (removed x on both sides)
