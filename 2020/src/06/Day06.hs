module Day06
(
  groupUniqueAnswers,
  groupUniqueAnswersForEveryone,
  countAnyoneYeses,
  countEveryoneYeses,
) where

import qualified Data.Set as Set
import Data.List.Split

type Answers = [Char]
type Group = [Answers]

groupUniqueAnswers :: Group -> Answers
groupUniqueAnswers allAnswers = Set.toAscList $ foldl Set.union Set.empty toUniques
  where
    toUniques = map Set.fromList allAnswers

groupUniqueAnswersForEveryone :: Group -> Answers
groupUniqueAnswersForEveryone allAnswers = Set.toAscList $ foldl Set.intersection (head toUniques) toUniques
  where
    toUniques = map Set.fromList allAnswers

countAnyoneYeses :: [Group] -> Int
countAnyoneYeses groups = sum $ map (length . groupUniqueAnswers) groups

countEveryoneYeses :: [Group] -> Int
countEveryoneYeses groups = sum $ map (length . groupUniqueAnswersForEveryone) groups

splitToGroups :: String -> [Group]
splitToGroups = map words . splitOn "\n\n"

main :: IO ()
main = do
  input <- getContents
  let groups = splitToGroups input

  let yeses = countAnyoneYeses groups

  putStrLn $ "Sum of yeses is " ++ show yeses

  let everyoneYeses = countEveryoneYeses groups

  putStrLn $ "Sum of everyone agreed yeses is " ++ show everyoneYeses
