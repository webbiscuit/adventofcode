module Day02
(
  runProgram
) where

import Data.Vector  hiding (map, (++))
import Data.List.Split

type Source = Int
type Destination = Int
type Memory = Vector Int
data Operation = Add Source Source Destination | Multiply Source Source Destination | End deriving (Show)

generateProgram :: [Int] -> Vector Int
generateProgram = fromList

executeOperation :: Operation -> Memory -> Memory
executeOperation (Add s1 s2 d1) memory = memory // [(d1, (memory ! s1) + (memory ! s2))]
executeOperation (Multiply s1 s2 d1) memory = memory // [(d1, (memory ! s1) * (memory ! s2))]

toOperation :: Int -> Memory -> Operation
toOperation location memory = toOp $ memory ! location
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
      runLoop pos memory = handle (toOperation pos memory)
        where
          handle End = memory
          handle op = runLoop (pos + 4) (executeOperation op memory)

putCode :: Memory -> Destination -> Int -> Memory
putCode memory destination value = memory // [(destination, value)]

parseInput :: String -> Memory
parseInput = fromList . map read . splitOn ","

main = do
  input <- getLine

  let program = parseInput input
  let programAlarm1202 = program // [(1,12), (2,2)]
  let result = runProgram programAlarm1202 ! 0 

  putStrLn ("Value at position 0 is " ++ show result)
  