module Day07
(
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

calculateFeedbackThrusterSignal :: Program  -> [Int] -> Int
calculateFeedbackThrusterSignal program seeds = cyclePrograms startPrograms 0
  where  
    startPrograms = seedComputers (loadProgram program) seeds

seedComputers :: Computer -> [Int] -> [Computer]
seedComputers p = map (\i -> runProgram p [i])

chainPrograms :: [Computer] -> Int -> [Computer]
chainPrograms [] n = []
chainPrograms (c:cs) n = newRun : chainPrograms cs (head $ getOutput newRun)
  where 
    newRun = runProgram c [n]

cyclePrograms :: [Computer] -> Int -> Int
cyclePrograms cs n = if isDone cycledPrograms then getLastOutput else cyclePrograms cycledPrograms getLastOutput
  where 
    cycledPrograms = chainPrograms cs n
    isDone cx = isStopped $ last cx
    getLastOutput = head $ getOutput $ last cycledPrograms

main = do
  input <- getLine

  let program = parseInput input
  let sequences = permutations [0,1,2,3,4]
  let maxThrusterSignal = maximum $ map (calculateThrusterSignal program) sequences

  putStrLn ("Max thruster signal is " ++ show maxThrusterSignal)

  let sequences = permutations [5,6,7,8,9]
  let maxThrusterSignal = maximum $ map (calculateFeedbackThrusterSignal program) sequences

  putStrLn ("Max feedback thruster signal is " ++ show maxThrusterSignal)

