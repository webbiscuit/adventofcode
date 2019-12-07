module Day05
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
data Instruction = 
  Add Source Source Destination | 
  Multiply Source Source Destination | 
  PutInput Destination |
  PutOutput Destination |
  End deriving Show
data Mode = Position | Immediate deriving Show
type Input = Int
type Output = [Int]

executeInstruction :: Instruction -> Memory -> Memory
executeInstruction (Add s1 s2 d1) memory = memory // [(d1, (memory ! s1) + (memory ! s2))]
executeInstruction (Multiply s1 s2 d1) memory = memory // [(d1, (memory ! s1) * (memory ! s2))]

executeInput :: Instruction -> Memory -> Input -> Memory
executeInput (PutInput destination) memory input = memory // [(destination, input)]

executeOutput :: Instruction -> Memory -> Output -> Output
executeOutput (PutOutput destination) memory output = output ++ [memory ! destination]

toInstruction :: Int -> Memory -> Instruction
toInstruction location memory = toOp $ memory ! location
  where 
    toOp 1 = Add address1 address2 address3 
    toOp 2 = Multiply address1 address2 address3
    toOp 3 = PutInput address1
    toOp 4 = PutOutput address1
    toOp 99 = End
    address1 = memory ! (location + 1)
    address2 = memory ! (location + 2)
    address3 = memory ! (location + 3)

toMode :: Int -> Mode
toMode 0 = Position
toMode 1 = Immediate

runProgram :: Memory -> Int -> (Memory, Output)
runProgram program input = runLoop 0 program input []
    where 
      runLoop :: Int -> Memory -> Input -> Output -> (Memory, Output)
      runLoop pos memory input output = handle (toInstruction pos memory)
        where
          handle End = (memory, output)
          handle instr@(PutInput _) = runLoop (pos + 2) (executeInput instr memory input) input output
          handle instr@(PutOutput _) = runLoop (pos + 2) memory input (executeOutput instr memory output)
          handle instr = runLoop (pos + 4) (executeInstruction instr memory) input output

putCode :: Memory -> Destination -> Int -> Memory
putCode memory destination value = memory // [(destination, value)]

parseInput :: String -> Memory
parseInput = fromList . map read . splitOn ","

main = do
  input <- getLine

  let program = parseInput input
  let (_,diagnosticCode) = runProgram program 1

  putStrLn ("Diagnostic code is " ++ show diagnosticCode)
