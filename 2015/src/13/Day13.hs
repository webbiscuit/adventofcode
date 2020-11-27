module Day13
(
  -- countNumbers
) where

import Text.Regex.PCRE
import Data.List
-- import Data.Graph.Inductive
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)

type Name = String
type Happiness = Int
type SeatingArrangement = (Name, Name, Happiness)
newtype NodeLabel = NodeLabel Name deriving(Show)

-- genGraph :: [SeatingArrangement] -> Gr NodeLabel Happiness
-- genGraph seatingPlan = mkGraph nodes edges
--   where
--     nodes = (\(n,i) -> (i, NodeLabel n)) <$> zip (getUniqueNames seatingPlan) [1..]
--     edges = [("David","Alice", 12)]

-- buildGraph = run_ empty $
--   do insMapNodeM 'a'
--      insMapNodeM 'b'
--      insMapNodeM 'c'
--      insMapEdgesM [
--       ('a', 'b', "right"),
--       ('b', 'a', "left"),
--       ('b', 'c', "down"),
--       ('c', 'a', "up")]

getUniqueNames :: [SeatingArrangement] -> [Name]
getUniqueNames = nub . foldl (\acc name -> getName name : acc) []

getPermutations :: [Name] -> [[Name]]
getPermutations = permutations

--getSeatingPairs :: [Name] -> [(Name,Name)]


getName (name, _, _) = name

regex :: String
regex = "(\\S*) would (gain|lose) (\\d*) happiness units by sitting next to (\\S*)."

parsePotential :: String -> [SeatingArrangement]
parsePotential s = map parse $ s =~ regex
  where 
    parse [_,name1,"gain",units,name2] = (name1 :: Name, name2 :: Name, read units :: Happiness)
    parse [_,name1,"lose",units,name2] = (name1 :: Name, name2 :: Name, -read units :: Happiness)


test = "Alice would gain 54 happiness units by sitting next to Bob.\nDavid would lose 46 happiness units by sitting next to Alice."
a = parsePotential test

main = do
  input <- getLine

  putStrLn "O"

  --putStrLn ("The total of all numbers are " ++ (show numberCount))
  