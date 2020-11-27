module Day10
(
  findMostAsteroids,
  zappedAsteroid,
  parseMap,
  findBestMonitoringLocation
) where

import Data.List
import Data.Maybe
import Data.Function 

type Location = (Float, Float)
type Asteroids = [Location]
type Angle = Float

angle :: Location -> Location -> Angle
angle (x1,y1) (x2,y2) = angle'
  where angle' = atan2 (y1 - y2) (x1 - x2)

calculateAngles :: Location -> Asteroids -> [(Location,Angle)]
calculateAngles a = map (\a2 -> (a2, angle a a2))

findMostAsteroids :: Asteroids -> Int
findMostAsteroids ax = maximum $ map (length . findVisibleAngles) $ findAsteroidAngles ax

findBestMonitoringLocation:: Asteroids -> Location
findBestMonitoringLocation ax = ax !! ix
  where
    findVisibleAsteroidCounts = map (length . findVisibleAngles) $ findAsteroidAngles ax
    ix = fromJust $ elemIndex (maximum findVisibleAsteroidCounts) findVisibleAsteroidCounts

findAsteroidAngles :: Asteroids -> [[(Location, Angle)]]
findAsteroidAngles ax = map (\a -> calculateAngles a $ filter (/=a) ax) ax

findVisibleAngles :: [(Location,Angle)] -> [(Location,Angle)]
findVisibleAngles = nubBy ((==) `on` snd)

parseMap :: String -> Asteroids
parseMap s = concatMap findAsteroids (zip (lines s) [0..])
  where
    findAsteroids (columns, y) = foldl (\acc (c,x) -> if c == '#' then acc ++ [(x,y)] else acc) [] (zip columns [0..])

zappedAsteroid :: Asteroids -> Int -> Location
zappedAsteroid ax n = (8,2)

-- zapAsteroid :: Asteroids -> Angle -> 

x2 ax = calculateAngles bestLocation ax
  where
    bestLocation = findBestMonitoringLocation ax

laserStartDirection :: Angle
laserStartDirection = pi / 2 --angle (0,0) (0,-1)

test = do
  m <- readFile "map1.txt"
  let pm = parseMap m
  return pm

main = do
  input <- getContents
  let starMap = parseMap input
  let mostAsteroids = findMostAsteroids starMap

  putStrLn ("Most asteroids detected are " ++ show mostAsteroids)
