module Day07
(
  bagsContainingBag,
  parseBagRule
) where

import Data.List
import Data.Maybe

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp, spLength)

type BagRule = String
type Bag = String
type BagCount = Integer
type BagContainsBags = (Bag, Bag, BagCount) 

-- newtype NodeLabel = NodeLabel String 
newtype NodeLabel = NodeLabel String deriving(Show)

parseBagRule :: BagRule -> (Bag, [(Bag, BagCount)])
parseBagRule bagRule = 

genLabelIds :: [String] -> [(String, Int)]
genLabelIds labels = zip labels [1..]

lookupLabelId :: [(String, Int)] -> String -> Int
lookupLabelId labelIds k = snd $ fromJust $ find (\(a,_) -> a == k) labelIds

genEdge (from, to) labelIds = (from, to)

test = ["red","blue","Blak"]
testEdge = [("red","blue", 2),("blue","Blak", 3)]
test2 = genLabelIds ["a","b","c"]


-- genGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genGraph :: [Bag] -> [BagContainsBags] -> Gr NodeLabel BagCount
genGraph nodes edges = mkGraph genNodeLabels genEdges
  where
    labelIds = genLabelIds nodes
    genNodeLabels = map (\(label,id) -> (id, NodeLabel label)) labelIds
    genEdges = map (\(a,b,c) -> (lookupLabelId labelIds a, lookupLabelId labelIds b, c)) edges

bagsContainingBag :: [BagRule] -> Bag -> [Bag]
bagsContainingBag bagRules bag = []


main :: IO ()
main = do
  input <- getContents
  let bagColours = bagsContainingBag (words input) "shiny gold"

  putStrLn $ "Number of bag colours is " ++ show (length bagColours)
