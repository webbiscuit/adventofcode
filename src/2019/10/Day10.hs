module Day10
(
  findMostAsteroids,
  parseMap
) where

import Data.List

type Location = (Float, Float)
type Asteroids = [Location]
type Angle = Float

angle :: Location -> Location -> Angle
angle (x1,y1) (x2,y2) = angle'
  where angle' = atan2 (y1 - y2) (x1 - x2)

calculateAngles :: Location -> Asteroids -> [Angle]
calculateAngles a = map (`angle` a)

findMostAsteroids :: Asteroids -> Int
findMostAsteroids ax = maximum $ map length $ angles ax
   where 
    angles ax = map (\a -> nub $ calculateAngles a $ filter (/=a) ax) ax
    kindaEqual a b = abs (a - b) < delta
    delta = 0.00000000000001

parseMap :: String -> Asteroids
parseMap s = concatMap findAsteroids (zip (lines s) [0..])
  where
    findAsteroids (columns, y) = foldl (\acc (c,x) -> if c == '#' then acc ++ [(x,y)] else acc) [] (zip columns [0..])

main = do
  input <- getContents
  let starMap = parseMap input
  let mostAsteroids = findMostAsteroids starMap

  putStrLn ("Most asteroids detected are " ++ show mostAsteroids)
