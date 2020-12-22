{-# LANGUAGE FlexibleContexts #-}

module Day14
(
  parse,
  initialiseMemory,
  sumMemoryValues
) where

import Data.Bits
import Text.Regex.PCRE
import qualified Data.Map as Map

type MemoryLocation = Int
type Memory = Map.Map Int Int

data Instruction =
  Mask String |
  Memory MemoryLocation Int deriving (Show)

sumMemoryValues :: Memory -> Int
sumMemoryValues = Map.foldr (+) 0

initialiseMemory :: [Instruction] -> Memory
initialiseMemory ix = executeInstructions ix Map.empty ""

executeInstructions :: [Instruction] -> Memory -> String -> Memory
executeInstructions [] mem _ = mem
executeInstructions (instruction:ix) mem mask = executeInstruction instruction
  where
    executeInstruction (Mask newMask) = executeInstructions ix mem newMask
    executeInstruction (Memory loc n) = executeInstructions ix (Map.insert loc (applyMask n mask) mem) mask

applyMask :: Int -> String -> Int
applyMask n mask = foldl (\acc (i,b) -> if b=='1' then setBit acc i else if b=='0' then clearBit acc i else acc) n maskList
  where
    maskList = zip [0..] (reverse mask)

maskRegex :: String
maskRegex = "^mask = (\\w*)$"

memoryRegex :: String
memoryRegex = "^mem\\[(\\d*)\\] = (\\d*)$"

parse :: String ->  [Instruction]
parse s = map parseLine $ lines s
  where
    parseLine line
      | line =~ maskRegex = parseMask (line =~ maskRegex :: [[String]])
      | line =~ memoryRegex = parseMemory (line =~ memoryRegex :: [[String]])
    parseMask [[_, mask]] = Mask mask
    parseMemory [[_, loc, val]] = Memory (read loc :: MemoryLocation) (read val :: Int)

main = do
  input <- getContents

  let instructions = parse input
  let memory = initialiseMemory instructions
  let memorySum = sumMemoryValues memory

  putStrLn $ "Memory sum is " ++ show memorySum
