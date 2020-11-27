module Day05
(
  isNiceString,
  isNiceString2,
) where

import Data.List

isVowel :: Char -> Bool
isVowel c = elem c ("aeiouAEIOU" :: [Char])

hasSameCharacterSequencially :: String -> Bool
hasSameCharacterSequencially [] = False
hasSameCharacterSequencially (x:y:xs) = x == y || hasSameCharacterSequencially (y:xs)
hasSameCharacterSequencially _ = False

hasForbiddenSequence :: String -> Bool
hasForbiddenSequence s = any (`isInfixOf` s) ["ab","cd","pq","xy"]

toChunks :: String -> [String]
toChunks (x:y:xs) = (x : [y]) : toChunks (y:xs)
toChunks _ = []

appearsWithoutOverlap :: [String] -> Bool
appearsWithoutOverlap (x:y:xs) = x `elem` xs || appearsWithoutOverlap (y:xs)
appearsWithoutOverlap _ = False

mirrorsAroundACharacter :: String -> Bool
mirrorsAroundACharacter (x:y:z:xs) = x == z || mirrorsAroundACharacter (y:z:xs)
mirrorsAroundACharacter _ = False

countVowels :: String -> Int
countVowels = length . filter isVowel

isNiceString :: String -> Bool
isNiceString s = countVowels s >= 3 && hasSameCharacterSequencially s && not (hasForbiddenSequence s)

isNiceString2 :: String -> Bool
isNiceString2 s = appearsWithoutOverlap (toChunks s) && mirrorsAroundACharacter s

countNiceStrings :: (String -> Bool) -> [String] -> Int
countNiceStrings f = length . filter f

showNiceStrings :: Int -> String
showNiceStrings n = "Number of strings that are nice: " ++ show n

showNiceStrings2 :: Int -> String
showNiceStrings2 n = "Number of strings that are nice (v2): " ++ show n

main = do
  input <- getContents
  putStrLn $ showNiceStrings $ countNiceStrings isNiceString $ lines input
  putStrLn $ showNiceStrings2 $ countNiceStrings isNiceString2 $ lines input
