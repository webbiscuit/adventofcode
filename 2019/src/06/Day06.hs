module Day06
(
  countOrbits,
  countTransfers
) where

import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.List

type Mass = String;
type Orbits = Map.Map Mass Mass
type Source = Mass
type Target = Mass

countOrbits :: [(Mass, Mass)] -> Int
countOrbits orbits = foldl (\acc m -> acc + length (walk orbitMap m "COM")) 0 keys
  where 
    orbitMap = toOrbitMap orbits
    keys = Map.keys orbitMap

walk :: Orbits -> Source -> Target -> [Mass]
walk orbits source target = drop 1 $ takeWhile (/= target) (iterate (orbits  Map.!) source) ++ [target]

toOrbitMap :: [(Mass, Mass)] -> Orbits
toOrbitMap = Map.fromList . map (\(a,b) -> (b,a))

countTransfers :: [(Mass, Mass)] -> Source -> Target -> Int
countTransfers orbits from to = length (path1 \\ path2) + length (path2 \\ path1)
  where 
    orbitMap = toOrbitMap orbits
    path1 = walk orbitMap from "COM"
    path2 = walk orbitMap to "COM"

parseInput :: String -> (Mass,Mass)
parseInput = toOrbit . splitOn ")"
  where toOrbit [x,y] = (x,y)
  
main = do
  input <- getContents

  let orbits = map parseInput $ lines input
  let orbitsCount = countOrbits orbits
  
  putStrLn ("Total number of orbits is " ++ show orbitsCount)

  let transferCount = countTransfers orbits "YOU" "SAN"

  putStrLn ("Transfers to santa " ++ show transferCount)

