module Day05
(
  runProgram
) where

import Data.Vector (Vector, fromList, (!), (//))
import Data.List.Split
import Text.Printf

type Value = Int
type Destination = Int
type Memory = Vector Int
data Instruction = 
  Add Parameter Parameter Destination | 
  Multiply Parameter Parameter Destination | 
  PutInput Destination |
  PutOutput Parameter |
  End deriving Show
type Parameter = (Mode, Value)
data Mode = Position | Immediate deriving Show
type Input = Int
type Output = [Int]
type OpCode = Int

executeInstruction :: Instruction -> Memory -> Memory
executeInstruction (Add s1 s2 d1) memory = memory // [(d1, memoryLookup memory s1 + memoryLookup memory s2)]
executeInstruction (Multiply s1 s2 d1) memory = memory // [(d1, memoryLookup memory s1 * memoryLookup memory s2)]

memoryLookup :: Memory -> Parameter -> Value
memoryLookup memory (Position,address) = memory ! address
memoryLookup memory (Immediate,value) = value

executeInput :: Instruction -> Memory -> Input -> Memory
executeInput (PutInput destination) memory input = memory // [(destination, input)]

executeOutput :: Instruction -> Memory -> Output -> Output
executeOutput (PutOutput value) memory output = output ++ [memoryLookup memory value]

toOpCodeAndModes :: Int -> Memory -> (OpCode, [Mode])
toOpCodeAndModes location memory = (toOpCode, reverse toModes)
  where 
    padOpCode = printf "%05d" (memory ! location) :: String
    toOpCode = read $ drop 3 padOpCode
    toModes = map (\c -> toMode $ read [c]) $ take 3 padOpCode

toInstruction :: Int -> Memory -> Instruction
toInstruction location memory = toOp opCodeAndModes
  where 
    opCodeAndModes = toOpCodeAndModes location memory
    toOp (1, [m1,m2,_]) = Add (m1, lookup 1) (m2, lookup 2) (lookup 3) 
    toOp (2, [m1,m2,_]) = Multiply (m1, lookup 1) (m2, lookup 2) (lookup 3)
    toOp (3, _) = PutInput $ lookup 1
    toOp (4, m1:_) = PutOutput (m1, lookup 1)
    toOp (99, _) = End
    lookup delta = memory ! (location + delta)

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
