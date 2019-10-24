module Day03
(
  countHousesWithPresents,
  countHousesWithPresentsUsingRobot,
) where

import Data.List

data Position = Position {posx ::Int, posy :: Int} deriving (Show, Eq)
data Step = Step {stepx :: Int, stepy :: Int} deriving (Show)

makeStep :: Char -> Step
makeStep '<' = Step (-1) 0
makeStep '>' = Step 1 0
makeStep '^' = Step 0 1
makeStep 'v' = Step 0 (-1)
makeStep _ = error "Woah! Where are we going?"

countHousesWithPresents :: String -> Int
countHousesWithPresents input = length $ nub $ doWalk (Position 0 0) input

countHousesWithPresentsUsingRobot :: String -> Int
countHousesWithPresentsUsingRobot input = length $ nub $ doWalk (Position 0 0) (toOddIndexes input) ++ doWalk (Position 0 0) (toEvenIndexes input)

makeMove :: Position -> Step -> Position
makeMove startPos step = Position (posx startPos + stepx step) (posy startPos + stepy step)

toOddIndexes :: String -> String
toOddIndexes input = map snd . filter (odd . fst) $ zip [1..] input

toEvenIndexes :: String -> String
toEvenIndexes input = map snd . filter (even . fst) $ zip [1..] input

doWalk :: Position -> String -> [Position] 
doWalk startPos input = scanl makeMove startPos $ map makeStep input

showHousesWithPresents :: Int -> String
showHousesWithPresents h = "Houses that got presents: " ++ show h

showHousesWithPresentsUsingRobot :: Int -> String
showHousesWithPresentsUsingRobot h = "Houses that got presents using a robot: " ++ show h

main = do
  input <- getContents
  putStrLn $ showHousesWithPresents $ countHousesWithPresents input
  putStrLn $ showHousesWithPresentsUsingRobot $ countHousesWithPresentsUsingRobot input


