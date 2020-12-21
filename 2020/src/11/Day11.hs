module Day11
(
  parse,
  countOccupiedSeats,
  moveUntilStable,
) where

import qualified Data.Vector as V
import Data.List
import Data.Maybe

data SeatType = EmptySeat | OccupiedSeat | Floor deriving (Show, Eq)
type SeatLayout = V.Vector (V.Vector SeatType)
type SeatPosition= (Int, Int)

moveUntilStable :: SeatLayout -> SeatLayout
moveUntilStable layout
  | layout == nextLayout = layout
  | otherwise = moveUntilStable nextLayout
  where 
    nextLayout = nextSeatLayout layout

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

getNeighbours :: SeatLayout -> SeatPosition -> [SeatType]
getNeighbours layout pos = mapMaybe (getSeatAt layout) (getOffsets pos)

getSeatAt :: SeatLayout -> SeatPosition -> Maybe SeatType
getSeatAt seats (x, y) = do
  row <- seats V.!? x 
  seat <- row V.!? y
  return seat

getOffsets :: SeatPosition -> [SeatPosition]
getOffsets (x,y) = [(x, y + 1), (x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y - 1), (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]

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

  let occupiedSeats = length $ filter (==OccupiedSeat) $ concat $ V.map V.toList finalLayout

  putStrLn $ "Number of seats occupied are " ++ show occupiedSeats
