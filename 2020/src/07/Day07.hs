module Day07
(
  bagsContainingBag,
  parseBagRule
) where

import Data.List
import Data.Maybe
import Data.List.Split
import Text.Regex.PCRE

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Query.DFS

type BagRule = String
type Bag = String
type BagCount = Integer
type BagContainsBags = (Bag, Bag, BagCount) 

newtype NodeLabel = NodeLabel String deriving(Show)

parseBagRule :: BagRule -> (Bag, [(Bag, BagCount)])
parseBagRule bagRule = (sourceBag, targetBags)
  where
    sourceTargetSplit = splitOn " bags contain " bagRule
    sourceBag = sourceTargetSplit !! 0
    targetBags = concatMap parseTargetBags $ splitOn ", " $ init $ sourceTargetSplit !! 1

parseTargetBags :: String -> [(Bag, BagCount)]    
parseTargetBags targetBag = map parse (targetBag =~ "^(\\d) (\\w* \\w*)" :: [[String]])
  where 
    parse [_, n, name] = (name, read n :: BagCount)

genLabelIds :: [String] -> [(String, Int)]
genLabelIds labels = zip labels [1..]

lookupLabelId :: [(String, Int)] -> String -> Int
lookupLabelId labelIds k = snd $ fromJust $ find (\(a,_) -> a == k) labelIds

toNode :: (Bag, [(Bag, BagCount)]) -> Bag
toNode (bag, _) = bag 

toEdge:: (Bag, [(Bag, BagCount)]) -> [BagContainsBags]
toEdge (source,values) = map (\(t,c) -> (source, t, c)) values 

genGraph :: [Bag] -> [BagContainsBags] -> Gr NodeLabel BagCount
genGraph nodes edges = mkGraph genNodeLabels genEdges
  where
    labelIds = genLabelIds nodes
    genNodeLabels = map (\(label,id) -> (id, NodeLabel (show id ++ " " ++ label))) labelIds
    genEdges = map (\(a,b,c) -> (lookupLabelId labelIds a, lookupLabelId labelIds b, c)) edges

bagsContainingBag :: [BagRule] -> Bag -> [Int]
bagsContainingBag bagRules bag = tail $ rdfs [lookupLabelId labelIds bag] (mkGraphBag bagRules) 
  where
    parsed = map parseBagRule bagRules
    nodes = map toNode parsed
    labelIds = genLabelIds nodes

mkGraphBag :: [BagRule] -> Gr NodeLabel BagCount
mkGraphBag bagRules = graph
  where 
    graph = genGraph nodes edges
    parsed = map parseBagRule bagRules
    nodes = map toNode parsed
    edges = concatMap toEdge parsed

main :: IO ()
main = do
  input <- getContents
  let bagColours = bagsContainingBag (lines input) "shiny gold"

  putStrLn $ "Number of bag colours is " ++ show (length bagColours)

  -- let dot = showDot (fglToDot graph)
  -- writeFile "file.dot" dot
