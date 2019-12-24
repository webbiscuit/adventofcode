module Day07
(
  -- runProgram,
  calculateThrusterSignal,
  calculateFeedbackThrusterSignal
) where

import IntCodeComputer
import Data.List
import Data.List.Split

parseInput :: String -> Program
parseInput = map read . splitOn ","

calculateThrusterSignal :: Program -> [Int] -> Int
calculateThrusterSignal program = foldl (\acc amp -> head $ getOutput $ runProgram (loadProgram program) [amp, acc]) 0
  where
    getOutput (_,_,_,output) = output

calculateFeedbackThrusterSignal :: Program  -> [Int] -> Int
calculateFeedbackThrusterSignal program = foldl (\acc amp -> head $ getOutput $ runProgram (loadProgram program) [amp, acc]) 0
  where
    getOutput (_,_,_,output) = output

main = do
  input <- getLine

  let program = parseInput input
  let sequences = permutations [0,1,2,3,4]
  let maxThrusterSignal = maximum $ map (calculateThrusterSignal program) sequences

  putStrLn ("Max thruster signal is " ++ show maxThrusterSignal)

