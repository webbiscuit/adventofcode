module Day08
(
  findXmasWeakness
) where

import Data.List
import Data.Maybe

type Data = [Int]
type PreambleSize = Int

window :: Int -> [Int] -> [[Int]]
window n = foldr (zipWith (:)) (repeat []) . take n . tails

checksums :: [Int] -> [Int]
checksums = map (\(a:b:_) -> a + b) . combinations 2

combinations :: Int -> [Int] -> [[Int]]
combinations n = filter (\a  -> length a == n) . subsequences

findXmasWeakness :: Data -> PreambleSize -> Int
findXmasWeakness xs n = last $ fromJust $ find (\xxs -> not $ isChecksumValid (last xxs) (init xxs)) (window (n + 1) xs)
  where  
    isChecksumValid i preamble = i `elem` checksums preamble

-- c :: Data
-- c = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]

parse :: String -> Data
parse s = map (\n -> read n :: Int) $ lines s

main :: IO ()
main = do
  input <- getContents

  let portData = parse input
  let output = findXmasWeakness portData 25

  putStrLn $ "XMAS weakness is " ++ show output

