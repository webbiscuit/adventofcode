module Day03
(
  parseMap,
  isTreeOnSpace,
  countTreesHit,
  countTreesHitWithAllStrategies,
  countTreesHitWithStrategy
) where

type TreeMapRow = String 
type TreeMap = [TreeMapRow]
type X = Int
type Y = Int

isTreeOnSpace :: TreeMapRow -> X -> Bool
isTreeOnSpace row x = row !! wrappedX == '#'
  where
    wrappedX = x `mod` mapWidth
    mapWidth = length row

countTreesHit :: TreeMap -> Int
countTreesHit treeMap = length $ filter (uncurry isTreeOnSpace) mapWithXCoords
  where
    mapWithXCoords = zip treeMap [0, 3..]

countTreesHitWithAllStrategies :: TreeMap -> Int
countTreesHitWithAllStrategies treeMap = product strategies
  where 
    strategies = [
      countTreesHitWithStrategy 1 1 treeMap, 
      countTreesHitWithStrategy 3 1 treeMap,
      countTreesHitWithStrategy 5 1 treeMap,
      countTreesHitWithStrategy 7 1 treeMap,
      countTreesHitWithStrategy 1 2 treeMap]

countTreesHitWithStrategy :: X -> Y -> TreeMap -> Int
countTreesHitWithStrategy x y treeMap = length $ filter (uncurry isTreeOnSpace) mapWithXCoords
  where
    mapWithXCoords = zip (mapWithYCoords y) [0, x..]
    mapWithYCoords y = map head $ takeWhile (not . null) $ iterate (drop y) treeMap
    

parseMap :: String -> TreeMap
parseMap = lines

main :: IO ()
main = do
  input <- getContents
  let treeMap = parseMap input

  let treesHit = countTreesHit treeMap

  putStrLn $ "Number of trees hit is " ++ show treesHit

  let treesHitWithStrategies = countTreesHitWithAllStrategies treeMap

  putStrLn $ "Number of trees hit using combined strategies is " ++ show treesHitWithStrategies