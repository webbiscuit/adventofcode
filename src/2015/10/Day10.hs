module Day10
(
  lookAndSay
) where

import Data.List (group)

lookAndSay :: String -> String
lookAndSay s = concatMap toGroupSegment (countGroups s)
  where 
    countGroups s = zip (group s) (map length $ group s)
    toGroupSegment (a,b) = show b ++ [head a]

applyLookAndSay :: Int -> String -> String
applyLookAndSay n s = iterate lookAndSay s !! n

main = do
  input <- getLine

  putStrLn input

  let lookSay40 = applyLookAndSay 40 input
  let lookSay50 = applyLookAndSay 50 input

  putStrLn ("Look and say 40 times: " ++ show (length lookSay40))
  putStrLn ("Look and say 50 times: " ++ show (length lookSay50))