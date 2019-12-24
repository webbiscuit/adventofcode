module IntCodeComputer
(
  runProgram,
  loadProgram,
  Memory,
  MemoryPosition,
  Output
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
  Add Parameter Parameter Parameter | 
  Multiply Parameter Parameter Parameter | 
  PutInput Parameter |
  PutOutput Parameter |
  JumpIfTrue Parameter Parameter |
  JumpIfFalse Parameter Parameter |
  LessThan Parameter Parameter Parameter |
  Equals Parameter Parameter Parameter |
  AdjustRelativeBase Parameter |
  End deriving Show
type Parameter = (Mode, Value)
data Mode = Position | Immediate | Relative deriving Show
type Input = Int
type Output = [Int]
type OpCode = Int
type MemoryPosition = Int
type MemoryPointer = MemoryPosition
type RelativeBase = MemoryPosition
type Computer = (Memory, MemoryPointer, RelativeBase, Output)

executeInstruction :: Instruction -> Memory -> MemoryPosition -> Memory
executeInstruction (Add s1 s2 d1) memory relativePos = memory // [(destMemoryLookup relativePos d1, memoryLookup memory relativePos s1 + memoryLookup memory relativePos s2)]
executeInstruction (Multiply s1 s2 d1) memory relativePos = memory // [(destMemoryLookup relativePos d1, memoryLookup memory relativePos s1 * memoryLookup memory relativePos s2)]
executeInstruction (LessThan s1 s2 d1) memory relativePos = memory // [(destMemoryLookup relativePos d1, if memoryLookup memory relativePos s1 < memoryLookup memory relativePos s2 then 1 else 0)]
executeInstruction (Equals s1 s2 d1) memory relativePos = memory // [(destMemoryLookup relativePos d1, if memoryLookup memory relativePos s1 == memoryLookup memory relativePos s2 then 1 else 0)]

destMemoryLookup :: MemoryPosition -> Parameter -> Value
destMemoryLookup _ (Position,address) = address
destMemoryLookup relativePos (Relative,value) = relativePos + value

memoryLookup :: Memory -> MemoryPosition -> Parameter -> Value
memoryLookup memory _ (Position,address) = memory ! address
memoryLookup memory _ (Immediate,value) = value
memoryLookup memory relativePos (Relative,value) = memory ! (relativePos + value)

executeInput :: Instruction -> Memory -> MemoryPosition -> Input -> Memory
executeInput (PutInput value) memory relativePos input = memory // [(destMemoryLookup relativePos value, input)]

executeOutput :: Instruction -> Memory -> MemoryPosition -> Output -> Output
executeOutput (PutOutput value) memory relativePos output = output ++ [memoryLookup memory relativePos value]

executeJump :: Instruction -> Memory -> MemoryPosition -> MemoryPosition -> MemoryPosition
executeJump (JumpIfTrue p1 p2) memory relativePos initialPos = if memoryLookup memory relativePos p1 /= 0 then memoryLookup memory relativePos p2 else initialPos + 3
executeJump (JumpIfFalse p1 p2) memory relativePos initialPos = if memoryLookup memory relativePos p1 == 0 then memoryLookup memory relativePos p2 else initialPos + 3

executeAdjustRelativeBase ::Instruction -> Memory -> MemoryPosition -> MemoryPosition
executeAdjustRelativeBase (AdjustRelativeBase p) memory relativePos = relativePos + memoryLookup memory relativePos p

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
    toOp (1, [m1,m2,m3]) = Add (m1, lookup 1) (m2, lookup 2) (m3, lookup 3) 
    toOp (2, [m1,m2,m3]) = Multiply (m1, lookup 1) (m2, lookup 2) (m3, lookup 3)
    toOp (3, m1:_) = PutInput (m1, lookup 1)
    toOp (4, m1:_) = PutOutput (m1, lookup 1)
    toOp (5, [m1,m2,_]) = JumpIfTrue (m1, lookup 1) (m2, lookup 2)
    toOp (6, [m1,m2,_]) = JumpIfFalse (m1, lookup 1) (m2, lookup 2)
    toOp (7, [m1,m2,m3]) = LessThan (m1, lookup 1) (m2, lookup 2) (m3, lookup 3)
    toOp (8, [m1,m2,m3]) = Equals (m1, lookup 1) (m2, lookup 2) (m3, lookup 3)
    toOp (9, m1:_) = AdjustRelativeBase (m1, lookup 1)
    toOp (99, _) = End
    lookup delta = memory ! (location + delta)

toMode :: Int -> Mode
toMode 0 = Position
toMode 1 = Immediate
toMode 2 = Relative

loadProgram :: [Int] -> Computer
loadProgram program = (fromList program, 0, 0, [])

runProgram :: Computer -> [Input] -> Computer
runProgram (memory,pos,relativeBase,_) input = finalise $ runLoop (raiseMemory memory 10000, pos, relativeBase, []) input
  where 
    finalise (memory, pos, relativeBase, output) = (packMemory memory, pos, relativeBase, output)
    runLoop :: Computer -> [Input] -> Computer
    runLoop (memory, pos, relativeBase, output) input = handle (toInstruction pos memory)
      where
        handle End = (memory, pos, relativeBase, output)
        handle instr@(PutInput _) = if hasInput then doPut else halt
          where 
            hasInput = not (null input)
            doPut = runLoop (executeInput instr memory relativeBase (head input), pos + 2, relativeBase, output) (tail input)
            halt = (memory, pos, relativeBase, output)
        handle instr@(PutOutput _) = runLoop (memory, pos + 2, relativeBase, executeOutput instr memory relativeBase output) input
        handle instr@(JumpIfTrue _ _) = runLoop (memory, executeJump instr memory relativeBase pos, relativeBase, output) input
        handle instr@(JumpIfFalse _ _) = runLoop (memory, executeJump instr memory relativeBase pos, relativeBase, output) input
        handle instr@(AdjustRelativeBase _) = runLoop (memory, pos + 2, executeAdjustRelativeBase instr memory relativeBase, output) input
        handle instr = runLoop (executeInstruction instr memory relativeBase, pos + 4, relativeBase, output) input

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

main = do
  input <- getLine

  -- let program = parseInput input
  -- let boostResult = runProgram program [1]

  -- putStrLn ("Boost result is " ++ show boostResult)

  -- let distressSignal = runProgram program [2]

  putStrLn ("Distress signal is " ++ show "0")
