module Day10
(
  calculateJoltageDifferences,
  countJoltageDifference,
  parse,
  prepare,
) where

import Data.List

type Joltage = Int
type JoltageDifference = Int

finalJoltageItem :: Joltage
finalJoltageItem = 3

calculateJoltageDifferences :: [Joltage] -> [JoltageDifference]
calculateJoltageDifferences vx = calculateDiffs
  where
    sorted = sort vx
    calculateDiffs = zipWith (-) (drop 1 sorted) sorted

countJoltageDifference :: [JoltageDifference] -> JoltageDifference -> Int
countJoltageDifference vdx d = length $ filter (==d) vdx

prepare :: [Joltage] -> [Joltage]
prepare xs = 0 : xs ++ [maximum xs + finalJoltageItem]

parse :: String -> [Joltage]
parse s = map (\v -> read v :: Joltage) $ lines s

main :: IO ()
main = do
  input <- getContents

  let joltages = prepare $ parse input
  let diffs = calculateJoltageDifferences joltages
  let output = countJoltageDifference diffs 1 * countJoltageDifference diffs 3

  putStrLn $ "Joltages multplied is " ++ show output

  --https://leetcode.com/problems/climbing-stairs/solution/ ??
