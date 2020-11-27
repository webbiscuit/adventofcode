module Day03
(
  countHousesWithPresents,
  countHousesWithPresentsUsingRobot,
) where

import Data.List

type Position = (Int, Int)
type Step = (Int, Int)

makeStep :: Char -> Step
makeStep '<' = (-1, 0)
makeStep '>' = (1, 0)
makeStep '^' = (0, 1)
makeStep 'v' = (0, -1)
makeStep _ = (0, 0)

countHousesWithPresents :: String -> Int
countHousesWithPresents input = length $ nub $ doWalk (0, 0) input

countHousesWithPresentsUsingRobot :: String -> Int
countHousesWithPresentsUsingRobot input = length $ nub $ doWalk (0, 0) (odds input) ++ doWalk (0, 0) (evens input)

makeMove :: Position -> Step -> Position
makeMove (x,y) (dx,dy) = (x + dx, y + dy)

doWalk :: Position -> String -> [Position]
doWalk startPos input = scanl makeMove startPos $ map makeStep input

odds :: String -> String
odds (x:y:xs) = [y] ++ (odds xs)
odds _ = []

evens :: String -> String
evens (x:y:xs) = [x] ++ (evens xs)
evens _ = []

showHousesWithPresents :: Int -> String
showHousesWithPresents h = "Houses that got presents: " ++ show h

showHousesWithPresentsUsingRobot :: Int -> String
showHousesWithPresentsUsingRobot h = "Houses that got presents using a robot: " ++ show h

main = do
  input <- getContents
  putStrLn $ showHousesWithPresents $ countHousesWithPresents input
  putStrLn $ showHousesWithPresentsUsingRobot $ countHousesWithPresentsUsingRobot input


