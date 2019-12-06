module Day03
(
  parseWire,
  calculateClosestIntersection
) where

import Data.List
import Data.List.Split
import qualified Data.Set as Set

type Point = (Int,Int)
type Distance = Int
data Direction = L | R | U | D deriving Show
type WireMove = (Direction, Distance)

doStep ::  Direction -> Point -> Point
doStep L (x,y) = (x - 1, y)
doStep R (x,y) = (x + 1, y)
doStep U (x,y) = (x, y + 1)
doStep D (x,y) = (x, y - 1)

doSteps :: Direction -> Distance -> Point -> [Point]
doSteps dir steps = take steps . drop 1 . iterate (doStep dir)

start :: Point
start = (0,0)

a :: [WireMove]
a = [(R, 8), (U, 5), (L, 5), (D, 3)]

b :: [WireMove]
b = [(U, 7), (R, 6), (D, 4), (L, 4)]

applyWire :: [WireMove] -> [Point]
applyWire = foldl (\acc (dir,dist) -> acc ++ doSteps dir dist (last acc)) [start]

calculateManhattanDistance :: Point -> Distance
calculateManhattanDistance (x,y) = abs x + abs y

calculateClosestIntersection :: [WireMove] -> [WireMove] -> Distance
calculateClosestIntersection wire1 wire2 = minimum $ filter (>0) $ map calculateManhattanDistance intersections
  where 
    intersections = Set.toList $ wire1Points `Set.intersection` wire2Points
    wire1Points = Set.fromList $ applyWire wire1
    wire2Points = Set.fromList $ applyWire wire2

parseWire :: String -> [WireMove]
parseWire s = map toWireMove $ splitOn "," s
    where 
      toWireMove :: String -> WireMove
      toWireMove ('U':distance) = (U, read distance)
      toWireMove ('D':distance) = (D, read distance)
      toWireMove ('L':distance) = (L, read distance)
      toWireMove ('R':distance) = (R, read distance)

main = do
  wire1Input <- getLine
  let wire1 = parseWire wire1Input

  wire2Input <- getLine
  let wire2 = parseWire wire2Input

  let minDistance = calculateClosestIntersection wire1 wire2 

  putStrLn ("The wires intersect with a closest distance of " ++ show minDistance)