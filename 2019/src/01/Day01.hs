module Day01
(
  calculateFuel,
  calculateFuelWithFuelMass
) where

type Mass = Int
type Fuel = Int

calculateFuel :: Mass -> Fuel
calculateFuel m = m `div` 3 - 2

calculateFuelWithFuelMass :: Mass -> Fuel
calculateFuelWithFuelMass m = sum $ drop 1 $ takeWhile (>= 0) $ iterate calculateFuel m

parseInput :: [String] -> [Mass]
parseInput = map (\s -> read s :: Mass)

main = do
  input <- getContents

  let masses = parseInput $ lines input
  let fuelNeeded = sum $ map calculateFuel masses
  let fuelNeededWithFuelMass = sum $ map calculateFuelWithFuelMass masses

  putStrLn ("The total amount of fuel needed is " ++ show fuelNeeded)
  putStrLn ("The total amount of fuel needed taking into account fuel mass is " ++ show fuelNeededWithFuelMass)
  