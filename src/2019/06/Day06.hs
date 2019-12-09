module Day06
(
  countOrbits
) where

import qualified Data.Map.Strict as Map
import Data.List.Split

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

a= [
  ("COM","B"),
  ("B","C"),
  ("C","D"),
  ("D","E"),
  ("E","F"),
  ("B","G"),
  ("G","H"),
  ("D","I"),
  ("E","J"),
  ("J","K"),
  ("K","L")]
  
parseInput :: String -> (Mass,Mass)
parseInput = toOrbit . splitOn ")"
  where toOrbit [x,y] = (x,y)
  
main = do
  input <- getContents

  let orbits = map parseInput $ lines input
  let orbitsCount = countOrbits orbits
  
  putStrLn ("Total number of orbits is " ++ show orbitsCount)