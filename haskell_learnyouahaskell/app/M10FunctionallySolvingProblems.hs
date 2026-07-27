main :: IO ()
main = putStrLn $ show $ solveRPN "1 + 2"

solveRPN :: (Floating a, Read a) => String -> a
solveRPN input = head $ foldl foldFun [] (words input)
 where
  foldFun (a : b : rest) "+" = (a + b) : rest
  foldFun (a : b : rest) "-" = (a - b) : rest
  foldFun (a : b : rest) "*" = (a * b) : rest
  foldFun (a : b : rest) "/" = (a / b) : rest
  foldFun (x : y : ys) "^" = (y ** x) : ys
  foldFun (x : xs) "ln" = log x : xs
  foldFun xs "sum" = [sum xs]
  foldFun stack item = (read item) : stack

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = ([(Label, Int)], Int)

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let ((bestAPath, lenA), (bestBPath, lenB)) = foldl roadStep (([], 0), ([], 0)) roadSystem
   in if lenA <= lenB
        then (reverse bestAPath, lenA)
        else (reverse bestBPath, lenB)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep ((pathA, lenA), (pathB, lenB)) (Section a b c) =
  let
    straightA = lenA + a
    straightB = lenB + b
    crossA = lenB + c
    crossB = lenA + c
    bestA = if straightA <= crossA then ((A, a) : pathA, straightA) else ((C, c) : (B, b) : pathB, crossA)
    bestB = if straightB <= crossB then ((B, b) : pathB, straightB) else ((C, c) : (A, a) : pathA, crossB)
   in
    (bestA, bestB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n list = take n list : (groupsOf n $ drop n list)

readRoadSystem :: String -> RoadSystem
readRoadSystem str = map (\[a, b, c] -> Section a b c) $ groupsOf 3 $ map read $ words str
