{-# LANGUAGE MultiParamTypeClasses #-}

module Day09
(
  buildLocationMap,
  calculateRouteDistance,
  LocationDistance (LocationDistance)
) where

import Text.Regex.PCRE
import qualified Data.HashMap.Strict as Map
import Data.List

type Distance = Int
type Location = String
type LocationMap = Map.HashMap (Location, Location) Distance

data LocationDistance = LocationDistance {
  from :: Location,
  to :: Location,
  distance :: Distance
} deriving (Show)

type Route = [Location]

regex :: String
regex = "^(\\S+) to (\\S+) = (\\d+)$"

parse :: String -> [LocationDistance]
parse s = map (\[_,f,t,d] -> LocationDistance f t (read d :: Distance)) $ s =~ regex

buildLocationMap :: [LocationDistance] -> LocationMap
buildLocationMap = Map.fromList . concatMap (\l -> [ ((from l, to l), distance l), ((to l, from l), distance l) ])

getUniqueLocations :: LocationMap -> [Location]
getUniqueLocations = nub . map fst . Map.keys

allRoutes :: [Location] -> [Route]
allRoutes = permutations

calculateRouteDistance :: LocationMap -> Route -> Distance
calculateRouteDistance m (a:b:xs) = m Map.! (a,b) + calculateRouteDistance m (b:xs)
calculateRouteDistance m _ = 0

showRoute :: Route -> Distance -> String
showRoute r d = intercalate " -> " r ++ " = " ++ show d

calculateShortestDistance :: LocationMap -> [Route] -> Distance
calculateShortestDistance locationMap routes = minimum $ map (calculateRouteDistance locationMap) routes

calculateLongestDistance :: LocationMap -> [Route] -> Distance
calculateLongestDistance locationMap routes = maximum $ map (calculateRouteDistance locationMap) routes

main = do
  input <- getContents
  let locationMap = buildLocationMap $ parse input
  let routes = allRoutes $ getUniqueLocations locationMap
  let shortestDistance = calculateShortestDistance locationMap routes
  let longestDistance = calculateLongestDistance locationMap routes

  putStrLn ("Shortest Distance is " ++ show shortestDistance)
  putStrLn ("Longest Distance is " ++ show longestDistance)