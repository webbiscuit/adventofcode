module Day12
(
  parse,
  findPosition,
  findPositionUsingWaypoint,
  calculateManhattanDistance,
) where

import Prelude hiding (Right, Left)

type Distance = Int
type Angle = Int
type Position = (Int, Int)

data Instruction =
  North Distance  |
  South Distance  |
  East Distance  |
  West Distance  |
  Left Distance  |
  Right Distance  |
  Forward Distance deriving Show

data Facing = N | S | E | W deriving Show

calculateManhattanDistance :: Position -> Position -> Distance
calculateManhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findPosition :: [Instruction] -> Position
findPosition xs = fst $ foldl (flip move) ((0,0), E) xs
  where
    move (North dy) ((x,y), f) = ((x, y + dy), f)
    move (South dy) ((x,y), f) = ((x, y - dy), f)
    move (East dx) ((x,y), f) = ((x + dx, y), f) 
    move (West dx) ((x,y), f) = ((x - dx, y), f) 
    move (Forward dx) pos@((x,y), f) = move (toMoveInstruction f dx) pos
    move (Right dx) pos@((x,y), f) = ((x, y), turn f dx) 
    move (Left dx) pos@((x,y), f) = ((x, y), turn f (-dx)) 

toMoveInstruction :: Facing -> Distance -> Instruction
toMoveInstruction N = North
toMoveInstruction S = South
toMoveInstruction E = East
toMoveInstruction W = West

turn :: Facing -> Angle -> Facing
turn f a = fromAngle $ (toAngle f + a) `mod` 360
  where 
    toAngle N = 0
    toAngle S = 180
    toAngle E = 90
    toAngle W = 270
    fromAngle 0 = N
    fromAngle 180 = S
    fromAngle 90 = E
    fromAngle 270 = W

rotate :: Position -> Angle -> Position
rotate (x,y) 90 = (y, -x)
rotate (x,y) 180 = (-x, -y)
rotate (x,y) 270 = (-y, x)
rotate (x,y) (-90) = (-y, x)
rotate (x,y) (-180) = (-x, -y)
rotate (x,y) (-270) = (y, -x)


findPositionUsingWaypoint :: Position -> Position -> [Instruction] -> Position
findPositionUsingWaypoint startPos startWaypoint xs = fst $ foldl (flip move) (startPos, startWaypoint) xs
  where
    move (North dy) (p, (x,y)) = (p, (x, y + dy))
    move (South dy) (p, (x,y)) = (p, (x, y - dy))
    move (East dx) (p, (x,y)) = (p, (x + dx, y)) 
    move (West dx) (p, (x,y)) = (p, (x - dx, y)) 
    move (Forward dx) ((x,y), (wx, wy)) = ((x + (wx * dx), y + (wy * dx)), (wx, wy))
    move (Right dx) (p, w) = (p, rotate w dx)
    move (Left dx) (p, w) = (p, rotate w (-dx))

parse :: String ->  [Instruction]
parse s = map parseLine $ lines s
  where
    parseLine ('N':xs) = North $ toInt xs
    parseLine ('S':xs) = South $ toInt xs
    parseLine ('E':xs) = East $ toInt xs
    parseLine ('W':xs) = West $ toInt xs
    parseLine ('L':xs) = Left $ toInt xs
    parseLine ('R':xs) = Right $ toInt xs
    parseLine ('F':xs) = Forward $ toInt xs
    toInt n = read n :: Distance

main = do
  input <- getContents

  let instructions = parse input
  let position = findPosition instructions
  let distance = calculateManhattanDistance (0,0) position

  putStrLn $ "Manhattan distance at the end is " ++ show distance

  let position2 = findPositionUsingWaypoint (0,0) (10,1) instructions
  let distance2 = calculateManhattanDistance (0,0) position2

  putStrLn $ "Manhattan distance with rotation instructions is " ++ show distance2
