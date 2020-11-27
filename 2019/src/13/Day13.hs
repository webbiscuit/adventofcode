module Day13
(
  createGrid,
  countTiles,
  Tile (Ball)
) where

import IntCodeComputer

import qualified Data.Map.Strict as Map
import Data.List.Split

data Tile = Empty | Wall | Block | Paddle | Ball deriving Eq
type Grid = Map.Map Point Tile
type Point = (Int, Int)

toTile :: Int -> Tile
toTile 0 = Empty
toTile 1 = Wall
toTile 2 = Block
toTile 3 = Paddle
toTile 4 = Ball

countTiles :: Grid -> Tile -> Int
countTiles grid tile = foldl (\acc v -> if v == tile then acc + 1 else acc) 0 grid 

createGrid :: Output -> Grid
createGrid = Map.fromList . addTile
  where 
    addTile (x:y:z:xs) = ((x,y), toTile z) : addTile xs
    addTile _ = []

runGame :: Program -> Grid
runGame p = createGrid $ getOutput $ runProgram (loadProgram p) []

parseProgram :: String -> Program
parseProgram = map read . splitOn ","

main = do
  input <- getLine

  let program = parseProgram input
  let grid = runGame program

  let blockTiles = countTiles grid Block

  putStrLn ("Number of blocks is " ++ show blockTiles)
