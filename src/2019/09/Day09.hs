module Day09
(
  runProgram,
) where

import Data.Vector (Vector, fromList, (!), (//))
import qualified Data.Vector as V ((++), replicate, reverse, dropWhile)
import Data.List
import Data.List.Split
import Text.Printf

type Value = Int
type Destination = Int
type Memory = Vector Int
data Instruction = 
  Add Parameter Parameter Destination | 
  Multiply Parameter Parameter Destination | 
  PutInput Parameter |
  PutOutput Parameter |
  JumpIfTrue Parameter Parameter |
  JumpIfFalse Parameter Parameter |
  LessThan Parameter Parameter Destination |
  Equals Parameter Parameter Destination |
  AdjustRelativeBase MemoryPosition |
  End deriving Show
type Parameter = (Mode, Value)
data Mode = Position | Immediate | Relative deriving Show
type Input = Int
type Output = [Int]
type OpCode = Int
type MemoryPosition = Int

executeInstruction :: Instruction -> Memory -> MemoryPosition -> Memory
executeInstruction (Add s1 s2 d1) memory relativePos = memory // [(d1, memoryLookup memory relativePos s1 + memoryLookup memory relativePos s2)]
executeInstruction (Multiply s1 s2 d1) memory relativePos = memory // [(d1, memoryLookup memory relativePos s1 * memoryLookup memory relativePos s2)]
executeInstruction (LessThan s1 s2 d1) memory relativePos = memory // [(d1, if memoryLookup memory relativePos s1 < memoryLookup memory relativePos s2 then 1 else 0)]
executeInstruction (Equals s1 s2 d1) memory relativePos = memory // [(d1, if memoryLookup memory relativePos s1 == memoryLookup memory relativePos s2 then 1 else 0)]

memoryLookup :: Memory -> MemoryPosition -> Parameter -> Value
memoryLookup memory _ (Position,address) = memory ! address
memoryLookup memory _ (Immediate,value) = value
memoryLookup memory relativePos (Relative,value) = memory ! (relativePos + value)

executeInput :: Instruction -> Memory -> MemoryPosition -> Input -> Memory
executeInput (PutInput value) memory relativePos input = memory // [(snd value, input)]

executeOutput :: Instruction -> Memory -> MemoryPosition -> Output -> Output
executeOutput (PutOutput value) memory relativePos output = output ++ [memoryLookup memory relativePos value]

executeJump :: Instruction -> Memory -> MemoryPosition -> MemoryPosition -> MemoryPosition
executeJump (JumpIfTrue p1 p2) memory relativePos initialPos = if memoryLookup memory relativePos p1 /= 0 then memoryLookup memory relativePos p2 else initialPos + 3
executeJump (JumpIfFalse p1 p2) memory relativePos initialPos = if memoryLookup memory relativePos p1 == 0 then memoryLookup memory relativePos p2 else initialPos + 3

executeAdjustRelativeBase ::Instruction -> MemoryPosition -> MemoryPosition
executeAdjustRelativeBase (AdjustRelativeBase p) relativePos = relativePos + p

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
    toOp (3, m1:_) = PutInput (m1, lookup 1)
    toOp (4, m1:_) = PutOutput (m1, lookup 1)
    toOp (5, [m1,m2,_]) = JumpIfTrue (m1, lookup 1) (m2, lookup 2)
    toOp (6, [m1,m2,_]) = JumpIfFalse (m1, lookup 1) (m2, lookup 2)
    toOp (7, [m1,m2,_]) = LessThan (m1, lookup 1) (m2, lookup 2) (lookup 3)
    toOp (8, [m1,m2,_]) = Equals (m1, lookup 1) (m2, lookup 2) (lookup 3)
    toOp (9, _) = AdjustRelativeBase $ lookup 1
    toOp (99, _) = End
    lookup delta = memory ! (location + delta)

toMode :: Int -> Mode
toMode 0 = Position
toMode 1 = Immediate
toMode 2 = Relative

runProgram :: Memory -> [Input] -> (Memory, MemoryPosition, Output)
runProgram program input = resumeProgram program input 0

resumeProgram :: Memory -> [Input] -> MemoryPosition -> (Memory, MemoryPosition, Output)
resumeProgram program input pos = finalise $ runLoop pos (raiseMemory program 10000) input [] 0
  where 
    finalise (memory, pos, output) = (packMemory memory, pos, output)
    runLoop :: MemoryPosition -> Memory -> [Input] -> Output -> MemoryPosition -> (Memory, MemoryPosition, Output)
    runLoop pos memory input output relativePos = handle (toInstruction pos memory)
      where
        handle End = (memory, pos, output)
        handle instr@(PutInput _) = if hasInput then doPut else halt
          where 
            hasInput = not (null input)
            doPut = runLoop (pos + 2) (executeInput instr memory relativePos (head input)) (tail input) output relativePos
            halt = (memory, pos, output)
        handle instr@(PutOutput _) = runLoop (pos + 2) memory input (executeOutput instr memory relativePos output) relativePos
        handle instr@(JumpIfTrue _ _) = runLoop (executeJump instr memory relativePos pos) memory input output relativePos
        handle instr@(JumpIfFalse _ _) = runLoop (executeJump instr memory relativePos pos) memory input output relativePos
        handle instr@(AdjustRelativeBase _) = runLoop (pos + 2) memory input output (executeAdjustRelativeBase instr relativePos)
        handle instr = runLoop (pos + 4) (executeInstruction instr memory relativePos) input output relativePos

putCode :: Memory -> Destination -> Int -> Memory
putCode memory destination value = memory // [(destination, value)]

parseInput :: String -> Memory
parseInput = fromList . map read . splitOn ","

isStopped :: Memory -> MemoryPosition -> Bool
isStopped mem pos = (mem ! pos) == 99

raiseMemory :: Memory -> Int -> Memory
raiseMemory program amount = program V.++ V.replicate amount 0

packMemory :: Memory -> Memory
packMemory = V.reverse . V.dropWhile (==0) . V.reverse

p :: Memory
p = fromList [
  3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
  27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

main = do
  input <- getLine

  let program = parseInput input
  let boostResult = runProgram program [1]

  putStrLn ("Boost result is " ++ show boostResult)
