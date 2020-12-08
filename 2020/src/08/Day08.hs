module Day08
(
  readAccumulator,
  runProgramUntilLoop,
) where

type Argument = Int
data Instruction = 
  Acc Argument |
  Jmp Argument |
  Nop Argument deriving Show
type Program = [Instruction]
type Output = Int

readAccumulator :: Int -> Int
readAccumulator _ = 0

runProgramUntilLoop :: Program -> Output
runProgramUntilLoop program = runLoop 0 0 []
  where
    runLoop line acc runLines
      | line `elem` runLines = acc
      | otherwise = execute (program !! line) []
      where
        execute (Acc n) = runLoop (line + 1) (acc + n)
        execute (Jmp n) = runLoop (line + n) acc
        execute (Nop _) = runLoop line acc 

-- parseToInstruction :: String -> Int -> Instruction
parseToInstruction :: [String] -> Instruction
parseToInstruction ["acc", n] = Acc (toInt n)
parseToInstruction ["jmp", n] = Jmp (toInt n)
parseToInstruction ["nop", n] = Nop (toInt n)

toInt :: String -> Int
toInt ('+':n) = read n :: Int
toInt n = read n :: Int

parseToProgram :: String -> Program
parseToProgram s = map (parseToInstruction . words) (lines s)

l = ["nop +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"]
l1 = "acc 1"

main :: IO ()
main = do
  input <- getContents

  let program = parseToProgram input
  let output = runProgramUntilLoop program

  putStrLn $ "Accumulator is " ++ show (output)

  -- let dot = showDot (fglToDot graph)
  -- writeFile "file.dot" dot
