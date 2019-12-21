module Day12
(
  parseMoons,
  applySteps,
  calculateTotalEnergy
) where

import Text.Regex.PCRE
import Text.Printf
import Data.List

type Position = (Int,Int,Int)
type Velocity = (Int,Int,Int)
type Moon = (Position,Velocity)
type Step = Int
type Energy = Int

showMoon :: Moon -> String
showMoon ((x,y,z),(dx,dy,dz)) = printf "pos=<x=%2d, y=%2d, z=%2d>, vel=<x=%2d, y=%2d, z=%2d>" x y z dx dy dz

applySteps :: [Moon] -> Step -> [Moon]
applySteps moons n = last $ take (n + 1) $ iterate step moons

input :: String
input  = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"

calculateTotalEnergy :: [Moon] -> Energy
calculateTotalEnergy = foldl (\acc e -> acc + calculateEnergy e) 0

calculateEnergy :: Moon -> Energy
calculateEnergy ((x,y,z),(dx,dy,dz)) = potentialEnergy * kineticEnergy
  where
    potentialEnergy = abs x + abs y + abs z
    kineticEnergy = abs dx + abs dy + abs dz

step :: [Moon] -> [Moon]
step moons = map (\moon -> applyVelocity moon $ applyGravity moon positions) moons
  where
    positions = map fst moons

applyGravity :: Moon -> [Position] -> Velocity
applyGravity (position, startVelocity) otherPositions = apply position
  where
    apply (x,y,z) = foldl (\(accx,accy,accz) (x2,y2,z2) -> (accx + doCompare x2 x, accy + doCompare y2 y, accz + doCompare z2 z)) startVelocity otherPositions
    doCompare c1 c2 = signum (c1 - c2)

applyVelocity :: Moon -> Velocity -> Moon
applyVelocity ((x,y,z), _) (dx,dy,dz) = ((x + dx, y + dy, z + dz), (dx,dy,dz))

createMoon :: Position -> Moon
createMoon startPoint = (startPoint, (0,0,0))

parseMoons :: String -> [Moon]
parseMoons s = map parse (s =~ regex)
   where
    regex :: String
    regex = "^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>$"
    parse [_,x,y,z] = createMoon (read x :: Int, read y :: Int, read z :: Int)

main = do
  input <- getContents
  let moons = parseMoons input
  let step1000 = applySteps moons 1000
  let energy = calculateTotalEnergy step1000

  putStrLn ("Energy in the system is " ++ show energy)
