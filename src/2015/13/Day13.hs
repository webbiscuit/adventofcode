module Day13
(
  -- countNumbers
) where

import Text.Regex.PCRE
import Data.List

type Name = String
type Happiness = Int
type SeatingArrangement = (Name, Name, Happiness)

getUniqueNames :: [SeatingArrangement] -> [Name]
getUniqueNames = nub . foldl (\acc name -> getName name : acc) []

getName (name, _, _) = name

regex :: String
regex = "(\\S*) would (gain|lose) (\\d*) happiness units by sitting next to (\\S*)."

parsePotential :: String -> [SeatingArrangement]
parsePotential s = map parse $ s =~ regex
  where 
    parse [_,name1,"gain",units,name2] = (name1 :: Name, name2 :: Name, read units :: Happiness)
    parse [_,name1,"lose",units,name2] = (name1 :: Name, name2 :: Name, -read units :: Happiness)


test = "Alice would gain 54 happiness units by sitting next to Bob.\nDavid would lose 46 happiness units by sitting next to Alice."

main = do
  input <- getLine

  putStrLn "O"

  --putStrLn ("The total of all numbers are " ++ (show numberCount))
  