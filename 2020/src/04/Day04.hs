module Day04
(
  isValidPassport,
  parsePasswordPolicies,
  validPassports,
  validPassports2,
  isByrValid,
  isHgtValid,
  isHclValid,
  isEclValid,
  isPidValid,
) where

import Data.List.Split
import Text.Regex.PCRE

type Key = String
type Val = String
type KeyVal = (Key, Val)

isByrValid :: Maybe String -> Bool
isByrValid (Just byrString) = byr >= 1920 && byr <= 2002
  where
    byr = read byrString :: Int
isByrValid _ = False

isIyrValid :: Maybe String -> Bool
isIyrValid (Just iyrString) = iyr >= 2010 && iyr <= 2020
  where
    iyr = read iyrString :: Int
iyrString _ = False

isEyrValid :: Maybe String -> Bool
isEyrValid (Just eyrString) = eyr >= 2020 && eyr <= 2030
  where
    eyr = read eyrString :: Int
eyrString _ = False

isHgtValid :: Maybe String -> Bool
isHgtValid (Just hgt) 
  | (length parse) > 0 = lookupUnit unit num
  | otherwise = False
  where
    parse = hgt =~ "^(\\d*)(in|cm)$" :: [[String]]
    [_, numStr, unit] = head parse
    num = read numStr :: Int
    lookupUnit "cm" n = n >= 150 && n <= 193 
    lookupUnit "in" n = n >= 59 && n <= 76 
isHgtValid _ = False

isHclValid :: Maybe String -> Bool
isHclValid (Just hcl) = hcl =~ "^#[0-9a-f]{6}$"
isHclValid _ = False

isEclValid :: Maybe String -> Bool
isEclValid (Just ecl) = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isEclValid _ = False

isPidValid :: Maybe String -> Bool
isPidValid (Just pid) = pid =~ "^\\d{9}$"
isPidValid _ = False

compulsoryFields :: [String]
compulsoryFields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

isValidPassport :: [KeyVal] -> Bool
isValidPassport keyvals = all (`elem` keys) compulsoryFields
  where keys = map fst keyvals

isValidPassport2 :: [KeyVal] -> Bool
isValidPassport2 keyvals = all (`elem` keys) compulsoryFields && valuesValid
  where 
    keys = map fst keyvals
    valuesValid = (isByrValid $ lookup "byr" keyvals) 
      && (isIyrValid $ lookup "iyr" keyvals) 
      && (isEyrValid $ lookup "eyr" keyvals) 
      && (isEclValid $ lookup "ecl" keyvals) 
      && (isHclValid $ lookup "hcl" keyvals) 
      && (isPidValid $ lookup "pid" keyvals)
      && (isHgtValid $ lookup "hgt" keyvals)

-- lookup :: [KeyVal] -> Key -> Val
lookupX kvs key = snd $ head $ filter (\(k,v) -> k == key) kvs

splitToPassportText :: String -> [String]
splitToPassportText = splitOn "\n\n"

parsePasswordPolicies :: String -> [KeyVal]
parsePasswordPolicies s = map parse (s =~ regex)
   where
    regex :: String
    regex = "(\\w{3}):(#?\\w*)"
    parse [_,key,val] = (key, val) :: KeyVal

validPassports :: String -> [[KeyVal]]
validPassports input = filter isValidPassport keyvals
  where
    passports = splitToPassportText input
    keyvals = map parsePasswordPolicies passports

validPassports2 :: String -> [[KeyVal]]
validPassports2 input = filter isValidPassport2 keyvals
  where
    passports = splitToPassportText input
    keyvals = map parsePasswordPolicies passports

main :: IO ()
main = do
  input <- getContents

  let validPassportCount = length $ validPassports input

  putStrLn $ "Valid passports " ++ show validPassportCount

  let validPassportCount2 = length $ validPassports2 input

  putStrLn $ "Valid passports v2 " ++ show validPassportCount2
