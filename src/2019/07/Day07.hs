module Day07
(
  runProgram,
  calculateThrusterSignal,
  calculateFeedbackThrusterSignal
) where

import Data.Vector (Vector, fromList, (!), (//))
import Data.List
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
  JumpIfTrue Parameter Parameter |
  JumpIfFalse Parameter Parameter |
  LessThan Parameter Parameter Destination |
  Equals Parameter Parameter Destination |
  End deriving Show
type Parameter = (Mode, Value)
data Mode = Position | Immediate deriving Show
type Input = Int
type Output = [Int]
type OpCode = Int
type MemoryPosition = Int

executeInstruction :: Instruction -> Memory -> Memory
executeInstruction (Add s1 s2 d1) memory = memory // [(d1, memoryLookup memory s1 + memoryLookup memory s2)]
executeInstruction (Multiply s1 s2 d1) memory = memory // [(d1, memoryLookup memory s1 * memoryLookup memory s2)]
executeInstruction (LessThan s1 s2 d1) memory = memory // [(d1, if memoryLookup memory s1 < memoryLookup memory s2 then 1 else 0)]
executeInstruction (Equals s1 s2 d1) memory = memory // [(d1, if memoryLookup memory s1 == memoryLookup memory s2 then 1 else 0)]

memoryLookup :: Memory -> Parameter -> Value
memoryLookup memory (Position,address) = memory ! address
memoryLookup memory (Immediate,value) = value

executeInput :: Instruction -> Memory -> Input -> Memory
executeInput (PutInput destination) memory input = memory // [(destination, input)]

executeOutput :: Instruction -> Memory -> Output -> Output
executeOutput (PutOutput value) memory output = output ++ [memoryLookup memory value]

executeJump :: Instruction -> Memory -> MemoryPosition -> MemoryPosition
executeJump (JumpIfTrue p1 p2) memory initialPos = if memoryLookup memory p1 /= 0 then memoryLookup memory p2 else initialPos + 3
executeJump (JumpIfFalse p1 p2) memory initialPos = if memoryLookup memory p1 == 0 then memoryLookup memory p2 else initialPos + 3

toOpCodeAndModes :: MemoryPosition -> Memory -> (OpCode, [Mode])
toOpCodeAndModes location memory = (toOpCode, reverse toModes)
  where 
    padOpCode = printf "%05d" (memory ! location) :: String
    toOpCode = read $ drop 3 padOpCode
    toModes = map (\c -> toMode $ read [c]) $ take 3 padOpCode

toInstruction :: MemoryPosition -> Memory -> Instruction
toInstruction location memory = toOp opCodeAndModes
  where 
    opCodeAndModes = toOpCodeAndModes location memory
    toOp (1, [m1,m2,_]) = Add (m1, lookup 1) (m2, lookup 2) (lookup 3) 
    toOp (2, [m1,m2,_]) = Multiply (m1, lookup 1) (m2, lookup 2) (lookup 3)
    toOp (3, _) = PutInput $ lookup 1
    toOp (4, m1:_) = PutOutput (m1, lookup 1)
    toOp (5, [m1,m2,_]) = JumpIfTrue (m1, lookup 1) (m2, lookup 2)
    toOp (6, [m1,m2,_]) = JumpIfFalse (m1, lookup 1) (m2, lookup 2)
    toOp (7, [m1,m2,_]) = LessThan (m1, lookup 1) (m2, lookup 2) (lookup 3)
    toOp (8, [m1,m2,_]) = Equals (m1, lookup 1) (m2, lookup 2) (lookup 3)
    toOp (99, _) = End
    lookup delta = memory ! (location + delta)

toMode :: Int -> Mode
toMode 0 = Position
toMode 1 = Immediate

runProgram :: Memory -> [Input] -> (Memory, MemoryPosition, Output)
runProgram program input = runLoop 0 program input []
    where 
      runLoop :: MemoryPosition -> Memory -> [Input] -> Output -> (Memory, MemoryPosition, Output)
      runLoop pos memory input output = handle (toInstruction pos memory)
        where
          handle End = (memory, pos, output)
          handle instr@(PutInput _) = runLoop (pos + 2) (executeInput instr memory (head input)) (tail input) output
          handle instr@(PutOutput _) = runLoop (pos + 2) memory input (executeOutput instr memory output)
          handle instr@(JumpIfTrue _ _) = runLoop (executeJump instr memory pos) memory input output
          handle instr@(JumpIfFalse _ _) = runLoop (executeJump instr memory pos) memory input output
          handle instr = runLoop (pos + 4) (executeInstruction instr memory) input output

putCode :: Memory -> Destination -> Int -> Memory
putCode memory destination value = memory // [(destination, value)]

parseInput :: String -> Memory
parseInput = fromList . map read . splitOn ","

calculateThrusterSignal :: Memory -> [Int] -> Int
calculateThrusterSignal program = foldl (\acc amp -> head $ getOutput $ runProgram program [amp, acc]) 0
  where
    getOutput (_,_,output) = output

calculateFeedbackThrusterSignal :: Memory -> [Int] -> Int
calculateFeedbackThrusterSignal program = foldl (\acc amp -> head $ getOutput $ runProgram program [amp, acc]) 0
  where
    getOutput (_,_,output) = output

main = do
  input <- getLine

  let program = parseInput input
  let sequences = permutations [0,1,2,3,4]
  let maxThrusterSignal = maximum $ map (calculateThrusterSignal program) sequences

  putStrLn ("Max thruster signal is " ++ show maxThrusterSignal)

