module Day01
(
  find2020Expense,
  find2020TriplesExpense,
) where

type Expense = Int
type Target = Int

find2020Expense :: [Expense] -> Expense
find2020Expense = findExpense 2020

find2020TriplesExpense :: [Expense] -> Expense
find2020TriplesExpense = findTriplesExpense 2020

findExpense :: Target -> [Expense] -> Expense
findExpense t (x:xs)
  | needle `elem` xs = x * needle
  | otherwise = findExpense t xs
  where
    needle = t - x  
findExpense _ _ = 0

findTriplesExpense :: Target -> [Expense] -> Expense
findTriplesExpense t (x:xs)
  | found > 0 = found * x
  | otherwise = findTriplesExpense t xs
  where
    needle = t - x  
    found = findExpense needle xs
findTriplesExpense _ _ = 0

parseExpenseList :: [String] -> [Expense]
parseExpenseList = map read

main :: IO()
main = do
  input <- getContents
  let expenses = parseExpenseList $ lines input
  let expenses2020 = find2020Expense expenses

  putStrLn ("Expenses for 2020 as doubles are: " ++ show expenses2020)

  let expenses2020Triples = find2020TriplesExpense expenses

  putStrLn ("Expenses for 2020 as triples are: " ++ show expenses2020Triples)
