module Day08
(
  runProgram,
  healProgram,
  parseToProgram,
  ExitCode ( Eof, Infinity ),
) where

import Data.List
import Data.Maybe

type Argument = Int
data Instruction =
  Acc Argument |
  Jmp Argument |
  Nop Argument deriving Show
type Program = [Instruction]
type Output = (ExitCode, Int)
data ExitCode = Eof | Infinity deriving (Show, Eq)

runProgram :: Program -> Output
runProgram program = runLoop 0 0 []
  where
    runLoop line acc linesExecuted
      | line `elem` linesExecuted = (Infinity, acc)
      | line >= length program = (Eof, acc)
      | otherwise = execute (program !! line) (line : linesExecuted)
      where
        execute (Acc n) = runLoop (line + 1) (acc + n)
        execute (Jmp n) = runLoop (line + n) acc
        execute (Nop _) = runLoop (line + 1) acc

parseToInstruction :: [String] -> Instruction
parseToInstruction ["acc", n] = Acc (toInt n)
parseToInstruction ["jmp", n] = Jmp (toInt n)
parseToInstruction ["nop", n] = Nop (toInt n)

toInt :: String -> Int
toInt ('+':n) = read n :: Int
toInt n = read n :: Int

parseToProgram :: String -> Program
parseToProgram s = map (parseToInstruction . words) (lines s)

flipInstruction :: Instruction -> Instruction
flipInstruction (Jmp n) = Nop n
flipInstruction (Nop n) = Jmp n
flipInstruction i =  i

healProgram :: Program -> Output
healProgram program = fromJust $ find (\(code,_) -> code == Eof) $ map runProgram allPrograms
   where 
    allPrograms = zipWith patchProgram [1..] $ replicate (length program) (programLines program)
    programLines = zip [1..]
    patchProgram flipLine = map (\(ln,p) -> if ln == flipLine then flipInstruction p else p) 

main :: IO ()
main = do
  input <- getContents

  let program = parseToProgram input
  let output = runProgram program

  putStrLn $ "Accumulator before loop is " ++ show (snd output)

  let healedOutput = healProgram program
  putStrLn $ "Accumulator with healed program is " ++ show (snd healedOutput)
