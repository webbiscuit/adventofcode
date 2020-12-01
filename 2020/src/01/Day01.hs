module Day01
(
  find2020Expense,
  find2020TriplesExpense,
) where

import Data.Maybe

type Expense = Int
type Target = Int

find2020Expense :: [Expense] -> Maybe Expense
find2020Expense = findExpense 2020

find2020TriplesExpense :: [Expense] -> Maybe Expense
find2020TriplesExpense = findTriplesExpense 2020

findExpense :: Target -> [Expense] -> Maybe Expense
findExpense t (x:xs)
  | needle `elem` xs = Just $ x * needle
  | otherwise = findExpense t xs
  where
    needle = t - x  
findExpense _ _ = Nothing

findTriplesExpense :: Target -> [Expense] -> Maybe Expense
findTriplesExpense t (x:xs)
  | Just f <- found =  Just (f * x)
  | otherwise = findTriplesExpense t xs
  where
    needle = t - x  
    found = findExpense needle xs
findTriplesExpense _ _ = Nothing

parseExpenseList :: [String] -> [Expense]
parseExpenseList = map read

main :: IO()
main = do
  input <- getContents
  let expenses = parseExpenseList $ lines input
  let expenses2020 = fromMaybe 0 $ find2020Expense expenses

  putStrLn ("Expenses for 2020 as doubles are: " ++ show expenses2020)

  let expenses2020Triples = fromMaybe 0 $ find2020TriplesExpense expenses

  putStrLn ("Expenses for 2020 as triples are: " ++ show expenses2020Triples)
