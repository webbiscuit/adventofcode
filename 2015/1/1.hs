import Data.List
import Data.Maybe

isFloorUp :: Char -> Bool
isFloorUp f = f == '('

floorAccumulator :: Int -> Char -> Int
floorAccumulator acc f = if isFloorUp f then acc + 1 else acc - 1

countFloors :: String -> Int
countFloors = foldl floorAccumulator 0

isBasement :: Int -> Bool
isBasement f = f < 0

firstBasementIndex :: String -> Int
firstBasementIndex fs = fromJust $ elemIndex (-1) $ scanl floorAccumulator 0 fs

main = do
  input <- getLine
  let floorCount = show $ countFloors input
  let finalFloor =  "Final floor " ++ floorCount
  putStrLn finalFloor

  let firstEnteredBasement = show $ firstBasementIndex input
  let basement =  "Basement first at instruction " ++ firstEnteredBasement
  putStrLn basement
