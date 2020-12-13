module Day13
(
  parse,
  findEarliestBus,
  calcWaitForBus
) where

import Data.List.Split
import Data.List
import Data.Ord

type Time = Integer
type Bus = Integer

findBusTimes :: Time -> [Bus] -> [(Bus, Time)]
findBusTimes leaveTime = map (\b -> (b, findLeaveTime b))
  where
    findLeaveTime busTime = ceiling (fromInteger leaveTime / fromInteger busTime) * busTime 

findEarliestBus :: Time -> [Bus] -> (Bus, Time)
findEarliestBus time buses = minimumBy (comparing snd) $ findBusTimes time buses

calcWaitForBus :: Time -> (Bus, Time) -> Integer
calcWaitForBus time (bus, leaveTime) = (leaveTime - time) * bus

parse :: String ->  (Time, [Bus])
parse s = (leavingTime, buses)
  where
    parsedLines = lines s
    leavingTime = read (head parsedLines) :: Time
    buses = map (\b -> read b :: Bus) $ filter (/= "x") $ splitOn "," $ last parsedLines


main = do
  input <- getContents

  let (time, buses) = parse input
  let bus = findEarliestBus time buses
  let busWait = calcWaitForBus time bus

  putStrLn $ "Wait for bus * bus Id is " ++ show busWait
