-- main :: IO ()
-- main = do
--   putStrLn "Hello, what's your name?"
--   name <- getLine
--   putStrLn ("Hey " ++ name ++ ", you rock!")

-- main :: IO ()
-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main
--
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--   c <- getChar
--   if c /= ' '
--     then do
--       putChar c
--       main
--     else return ()

-----------

-- main = do
--   contents <- getContents
--   putStr (shortLinesOnly contents)
-- -- OR
-- main = interact shortLinesOnly
--
-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--   let allLines = lines input
--       shortLines = filter (\line -> length line < 10) allLines
--       result = unlines shortLines
--    in result

import System.IO

-- main = do
--   handle <- openFile "file.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- main = do
--   withFile
--     "file.txt"
--     ReadMode
--     ( \handle -> do
--         contents <- hGetContents handle
--         putStr contents
--     )

-- import Control.Monad (when)
-- import System.Random
--
-- main = do
--   gen <- getStdGen
--   askForNumber gen
--
-- askForNumber :: StdGen -> IO ()
-- askForNumber gen = do
--   let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
--   putStr "Which number in the range from 1 to 10 am I thinking of? "
--   numberString <- getLine
--   when (not $ null numberString) $ do
--     let number = read numberString
--     if randNumber == number
--       then putStrLn "You are correct!"
--       else putStrLn $ "Sorry, it was " ++ show randNumber
--     askForNumber newGen

-- import Data.ByteString qualified as S
-- import Data.ByteString.Lazy qualified as B
--
-- import System.Environment
-- import System.IO
-- import System.IO.Error
--
-- main = toTry `catch` handler
--
-- toTry :: IO ()
-- toTry = do (fileName:_) <- getArgs
--            contents <- readFile fileName
--            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--
-- handler :: IOError -> IO ()
-- handler e
--     | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--     | otherwise = ioError e
