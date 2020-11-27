{-# LANGUAGE PackageImports #-}
module Day04
(
  findHashThatMakesNZeroes,
) where

import qualified Data.ByteString.Char8 as C8
import "cryptohash" Crypto.Hash
import Data.List

md5 :: C8.ByteString -> Digest MD5
md5 = hash

startsWithNZeroes :: Int -> String -> Bool
startsWithNZeroes n s = take n (repeat '0') == (take n s)

showHash :: Int -> String -> String
showHash n h = "Lowest number it combines with to make a hash starting with " ++ (show n) ++ " zeroes: " ++ h

findHashThatMakesNZeroes :: Int -> String -> String
findHashThatMakesNZeroes z i = findIt 1
  where 
    findIt n | startsWithNZeroes z $ show . md5 . C8.pack $ i ++ (show n) = show n
             | otherwise = findIt (n + 1)

main = do
  input <- getContents
  putStrLn $ showHash 5 $ findHashThatMakesNZeroes 5 input
  putStrLn $ showHash 6 $ findHashThatMakesNZeroes 6 input
