module Day06
(
  countLightsOn,
  measureBrightness
) where

import Data.Array
import Text.Regex.PCRE

type Point = (Int, Int)
type Grid = Array Int Int

width :: Int
width = 1000

height :: Int
height = 1000

initial :: Grid
initial = array (1, size) [(i, 0) | i <- [1..size]]
  where size = width * height

coordsToPoints :: Point -> Point -> [Point]
coordsToPoints (x1, y1) (x2, y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

instructionRegex :: String
instructionRegex = "^(turn on|toggle|turn off) (\\d+),(\\d+) through (\\d+),(\\d+)$"

toIndex :: Point -> Int
toIndex (x, y) = 1 + (x * width + y)

turnOn :: Grid -> [Point] -> Grid
turnOn m px = (//) m [(toIndex (x, y), 1) | (x, y) <- px]

turnOff :: Grid -> [Point] -> Grid
turnOff m px = (//) m [(toIndex (x, y), 0) | (x, y) <- px]

toggle :: Grid -> [Point] -> Grid
toggle m px = (//) m [(ix, v) | (x, y) <- px, let ix = toIndex (x, y), let v = if (!) m ix == 1 then 0 else 1]

turnOn2 :: Grid -> [Point] -> Grid
turnOn2 m px = (//) m [(ix, 1 + (!) m ix) | (x, y) <- px, let ix = toIndex (x, y)]

turnOff2 :: Grid -> [Point] -> Grid
turnOff2 m px = (//) m [(ix, v - 1) | (x, y) <- px, let ix = toIndex (x, y), let v = (!) m ix, v > 0]

toggle2 :: Grid -> [Point] -> Grid
toggle2 m px = (//) m [(ix, 2 + (!) m ix) | (x, y) <- px, let ix = toIndex (x, y)]

stringToOperation :: String -> (Grid -> [Point] -> Grid)
stringToOperation "turn on" = turnOn
stringToOperation "turn off" = turnOff
stringToOperation "toggle" = toggle
stringToOperation err = error $ "Not an operation " ++ err

stringToOperation2 :: String -> (Grid -> [Point] -> Grid)
stringToOperation2 "turn on" = turnOn2
stringToOperation2 "turn off" = turnOff2
stringToOperation2 "toggle" = toggle2
stringToOperation2 err = error $ "Not an operation " ++ err

handleOperation :: Grid -> [String] -> Grid
handleOperation m [_, op, x1, y1, x2, y2] = stringToOperation op m points
  where 
    point1 = (read x1 :: Int, read y1 :: Int)
    point2 = (read x2 :: Int, read y2 :: Int)
    points = coordsToPoints point1 point2

handleOperation2 :: Grid -> [String] -> Grid
handleOperation2 m [_, op, x1, y1, x2, y2] = stringToOperation2 op m points
  where 
    point1 = (read x1 :: Int, read y1 :: Int)
    point2 = (read x2 :: Int, read y2 :: Int)
    points = coordsToPoints point1 point2

parse :: String -> [String]
parse s = head (s =~ instructionRegex :: [[String]])

lineToGrid :: Grid -> String -> Grid
lineToGrid m = handleOperation m . parse 

linesToGrid :: String -> Grid
linesToGrid = foldl lineToGrid initial . lines

lineToGrid2 :: Grid -> String -> Grid
lineToGrid2 m = handleOperation2 m . parse 

linesToGrid2 :: String -> Grid
linesToGrid2 = foldl lineToGrid2 initial . lines

countLightsOn :: String -> Int
countLightsOn = sum . linesToGrid

measureBrightness :: String -> Int
measureBrightness = sum . linesToGrid2

showLightsOn :: Int -> String
showLightsOn n = "There are " ++ show n ++ " lights on"

showBrightness :: Int -> String
showBrightness n = "Brightness is " ++ show n

main = do
  input <- getContents
  putStrLn $ showLightsOn $ countLightsOn input
  putStrLn $ showBrightness $ measureBrightness input
