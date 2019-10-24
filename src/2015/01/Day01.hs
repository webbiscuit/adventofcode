module Day01
(
  countFloors,
  firstBasementPosition
) where

import Data.List

isFloorUp :: Char -> Bool
isFloorUp f = f == '('

floorAccumulator :: Int -> Char -> Int
floorAccumulator acc f = if isFloorUp f then acc + 1 else acc - 1

countFloors :: String -> Int
countFloors = foldl floorAccumulator 0

isBasement :: Int -> Bool
isBasement f = f < 0

firstBasementPosition :: String -> Maybe Int
firstBasementPosition fs = elemIndex (-1) $ scanl floorAccumulator 0 fs

showFinalFloor :: Int -> String
showFinalFloor f = "Final floor " ++ (show f)

showFirstBasementPosition :: Maybe Int -> String
showFirstBasementPosition (Just p) = "Basement first at instruction " ++ (show p)
showFirstBasementPosition Nothing = "Never went to the basement"

main = do
  input <- getLine
  putStrLn $ showFinalFloor $ countFloors input
  putStrLn $ showFirstBasementPosition $ firstBasementPosition input

