module Day11
(
  parse,
  countOccupiedSeats,
  moveUntilStable,
  getViewableNeighbours,
  moveUntilStableUsingLooking,
  SeatType (EmptySeat, OccupiedSeat, Floor)
) where

import qualified Data.Vector as V
import Data.List
import Data.Maybe

data SeatType = EmptySeat | OccupiedSeat | Floor deriving (Show, Eq)
type SeatLayout = V.Vector (V.Vector SeatType)
type SeatPosition= (Int, Int)
type Direction= (Int, Int)

moveUntilStable :: SeatLayout -> SeatLayout
moveUntilStable layout
  | layout == nextLayout = layout
  | otherwise = moveUntilStable nextLayout
  where 
    nextLayout = nextSeatLayout layout

moveUntilStableUsingLooking :: SeatLayout -> SeatLayout
moveUntilStableUsingLooking layout
  | layout == nextLayout = layout
  | otherwise = moveUntilStableUsingLooking nextLayout
  where 
    nextLayout = nextVisibleSeatLayout layout

nextSeatLayout :: SeatLayout -> SeatLayout
nextSeatLayout layout = V.fromList $ map nextRow [0..(length layout - 1)]
  where
    nextRow x = V.fromList $ map (nextSeat layout) [(x,y) | y <- [0..length (layout V.! x) - 1]]

nextSeat :: SeatLayout -> SeatPosition -> SeatType
nextSeat layout pos
  | current == EmptySeat && occupiedCount == 0 = OccupiedSeat
  | current == OccupiedSeat && occupiedCount >= 4 = EmptySeat
  | otherwise = current
    where
      current = fromJust $ getSeatAt layout pos
      neighbours = getNeighbours layout pos
      occupiedCount = length $ filter (==OccupiedSeat) neighbours

nextVisibleSeatLayout :: SeatLayout -> SeatLayout
nextVisibleSeatLayout layout = V.fromList $ map nextRow [0..(length layout - 1)]
  where
    nextRow x = V.fromList $ map (nextVisibleSeat layout) [(x,y) | y <- [0..length (layout V.! x) - 1]]

nextVisibleSeat :: SeatLayout -> SeatPosition -> SeatType
nextVisibleSeat layout pos
  | current == EmptySeat && occupiedCount == 0 = OccupiedSeat
  | current == OccupiedSeat && occupiedCount >= 5 = EmptySeat
  | otherwise = current
    where
      current = fromJust $ getSeatAt layout pos
      neighbours = getViewableNeighbours layout pos
      occupiedCount = length $ filter (==OccupiedSeat) neighbours

getNeighbours :: SeatLayout -> SeatPosition -> [SeatType]
getNeighbours layout pos = mapMaybe (getSeatAt layout) (getOffsets pos)

getSeatAt :: SeatLayout -> SeatPosition -> Maybe SeatType
getSeatAt seats (x, y) = do
  row <- seats V.!? x 
  seat <- row V.!? y
  return seat

getOffsets :: SeatPosition -> [SeatPosition]
getOffsets (x,y) = [(x, y + 1), (x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y - 1), (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]

getSeatInDirectionAt :: SeatLayout -> SeatPosition -> Direction -> Maybe SeatType
getSeatInDirectionAt seats (x, y) (dx, dy) = checkDir (x + dx, y + dy)
  where
    checkDir pos@(x2, y2)
      | seatTile == Just Floor = checkDir (x2 + dx, y2 + dy)
      | otherwise = seatTile
      where
        seatTile = getSeatAt seats pos
 
getDirections :: [Direction]
getDirections = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

getViewableNeighbours :: SeatLayout -> SeatPosition -> [SeatType]
getViewableNeighbours layout pos = mapMaybe (getSeatInDirectionAt layout pos) getDirections

parse :: String -> SeatLayout
parse = V.fromList . map parseLine . words
  where
    parseLine = V.fromList . map toSeatType

makeMap :: SeatLayout -> String
makeMap = intercalate "\n" . V.toList . V.map toIconLine
  where
    toIconLine = V.toList . V.map toIcon

toSeatType 'L' = EmptySeat
toSeatType '.' = Floor
toSeatType '#' = OccupiedSeat

toIcon EmptySeat = 'L'
toIcon Floor = '.'
toIcon OccupiedSeat = '#'

countOccupiedSeats :: SeatLayout -> Int
countOccupiedSeats layout = length $ filter (==OccupiedSeat) $ concat $ V.map V.toList layout


main :: IO ()
main = do
  input <- getContents
  let layout = parse input
  let finalLayout = moveUntilStable layout

  let occupiedSeats = countOccupiedSeats finalLayout

  putStrLn $ "Number of seats occupied are " ++ show occupiedSeats

  let finalLayout2 = moveUntilStableUsingLooking layout

  let occupiedSeats2 = countOccupiedSeats finalLayout2

  putStrLn $ "Number of seats occupied using eyes are " ++ show occupiedSeats2
