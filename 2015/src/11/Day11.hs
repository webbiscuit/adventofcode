module Day11
(
  incrementPassword,
  incrementPasswordWithRequirements,
  requirement1,
  requirement2,
  requirement3
) where

import Data.Char
import Data.List

incrementPassword :: String -> String
incrementPassword = reverse . incrementString . reverse
  where 
    incrementString (x:xs) = if incrementChar x == 'a' then incrementChar x : incrementString xs else incrementChar x : xs
    incrementString _ = "a"

incrementPasswordWithRequirements :: String -> String    
incrementPasswordWithRequirements p = if passwordGood (incrementPassword p) then incrementPassword p else incrementPasswordWithRequirements (incrementPassword p)
    where 
      passwordGood p = requirement2 p && requirement3 p && requirement1 p

incrementChar :: Char -> Char
incrementChar 'z' = 'a'
incrementChar c = chr (ord c + 1)

-- Checks for a 3 run sequence
requirement1 :: String -> Bool
requirement1 (a:b:c:xs) = ((ord a + 2) == (ord b + 1) && (ord b + 1) == (ord c)) || requirement1 (b:c:xs)
requirement1 _ = False

-- Disallows invalid chars
requirement2 :: String -> Bool
requirement2 = not . any isDisallowedChar
    where 
      isDisallowedChar c = c `elem` ("iol" :: [Char])

-- Counts Pairs
requirement3 :: String -> Bool
requirement3 xs = (>=2) $ sum $ map (\g -> length g `div` 2) $ group xs


main = do
  input <- getLine

  putStrLn input
  let nextPass = incrementPasswordWithRequirements input
  let nextPass2 = incrementPasswordWithRequirements nextPass

  putStrLn ("Next password is " ++ nextPass)
  putStrLn ("After that it is " ++ nextPass2)