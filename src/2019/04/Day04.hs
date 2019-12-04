module Day04
(
  isValidPassword,
  isValidPasswordV2
) where

import Data.List.Split
import Data.List

type Range = (Int, Int)

isValidPassword :: Int -> Bool
isValidPassword n = hasAdjacentDigits password && adjacentNeverDecreases password
  where
    password = show n
    hasAdjacentDigits (x:y:xs) = (x == y) || hasAdjacentDigits (y:xs)
    hasAdjacentDigits _ = False 
    adjacentNeverDecreases (x:y:xs) = (x <= y) && adjacentNeverDecreases (y:xs)
    adjacentNeverDecreases _ = True 

isValidPasswordV2 :: Int -> Bool
isValidPasswordV2 n = isValidPassword n && hasAdjacentDoubleOnly password
  where
    password = show n
    hasAdjacentDoubleOnly = any (\g -> length g == 2) . group

findValidPasswords :: (Int -> Bool) -> Range -> [Int]
findValidPasswords validFunc range = filter validFunc [fst range..snd range]

parseInput :: String -> Range
parseInput = toPair . map read . splitOn "-"
    where toPair [x,y] = (x,y)

main = do
  input <- getLine

  let range = parseInput input
  let validPasswords = findValidPasswords isValidPassword range

  putStrLn ("Number of valid passwords " ++ show (length validPasswords))

  let validPasswords2 = findValidPasswords isValidPasswordV2 range

  putStrLn ("Number of valid passwords with extra rules " ++ show (length validPasswords2))