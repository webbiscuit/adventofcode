module Day05
(
  calculateSeatId,
  findMissingSeatId,
) where

import Data.Bits
import Data.List
import Data.Maybe

encode :: Char -> Bool
encode 'B' = True
encode 'F' = False
encode 'R' = True
encode 'L' = False
encode _ = False

calculateSeatId :: String -> Int
calculateSeatId ticket = foldl (\acc (i,b) -> if b then acc `setBit` i else acc) 0 $ zip [9,8..] bitmap
  where
    bitmap = map encode ticket

calculateSeatIds :: String -> [Int]
calculateSeatIds input = map calculateSeatId $ lines input

findMissingSeatId :: [Int] -> Maybe Int
findMissingSeatId seatIds = case find (uncurry(/=)) $ zip (sort seatIds) [firstSeat..] of
    (Just (a,b)) -> Just b
    _ -> Nothing
  where
    firstSeat = minimum seatIds 

main :: IO ()
main = do
  input <- getContents

  let seatIds = calculateSeatIds input

  let highestSeatId = maximum seatIds

  putStrLn $ "Highest seat ID is " ++ show highestSeatId

  let missingSeatId = fromMaybe 0 $ findMissingSeatId seatIds

  putStrLn $ "Missing seat ID is " ++ show missingSeatId