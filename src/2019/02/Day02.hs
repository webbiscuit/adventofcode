module Day02
(
  runProgram
) where

import Data.Vector (Vector, fromList, (!), (//))
import Data.List.Split

type Source = Int
type Destination = Int
type Memory = Vector Int
type Noun = Int
type Verb = Int
data Instruction = Add Source Source Destination | Multiply Source Source Destination | End deriving (Show)

executeInstruction :: Instruction -> Memory -> Memory
executeInstruction (Add s1 s2 d1) memory = memory // [(d1, (memory ! s1) + (memory ! s2))]
executeInstruction (Multiply s1 s2 d1) memory = memory // [(d1, (memory ! s1) * (memory ! s2))]

toInstruction :: Int -> Memory -> Instruction
toInstruction location memory = toOp $ memory ! location
  where 
    toOp 1 = Add address1 address2 address3 
    toOp 2 = Multiply address1 address2 address3
    toOp 99 = End
    address1 = memory ! (location + 1)
    address2 = memory ! (location + 2)
    address3 = memory ! (location + 3)

runProgram :: Memory -> Memory
runProgram = runLoop 0
    where 
      runLoop pos memory = handle (toInstruction pos memory)
        where
          handle End = memory
          handle instr = runLoop (pos + 4) (executeInstruction instr memory)

putCode :: Memory -> Destination -> Int -> Memory
putCode memory destination value = memory // [(destination, value)]

makeProgram :: Memory -> (Noun,Verb) -> Memory
makeProgram mem (noun,verb) = mem // [(1, noun), (2, verb)]

makeAllPrograms :: Memory -> [Memory]
makeAllPrograms seedProgram = scanl makeProgram seedProgram [(a,b) | a <- [0..99], b <- [0..99]]

findProgram :: Memory -> Int -> Memory
findProgram seed goal = head $ filter (\m -> m ! 0 == goal) $ map runProgram $ makeAllPrograms seed

parseInput :: String -> Memory
parseInput = fromList . map read . splitOn ","

main = do
  input <- getLine

  let program = parseInput input
  let programAlarm1202 = program // [(1,12), (2,2)]
  let result = runProgram programAlarm1202 ! 0 

  putStrLn ("Value at position 0 is " ++ show result)

  let foundProgram = findProgram program 19690720
  let nounVerb = (foundProgram ! 1) * 100 + foundProgram ! 2

  putStrLn ("Noun/Verb code for 19690720 is " ++ show nounVerb)