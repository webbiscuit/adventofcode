module Day02
(
  isValidPassword,
  parsePasswordPolicies,
  isValidPasswordV2,
) where
import Text.Regex.PCRE


type PasswordPolicy = (Int, Int, Char, String)

isValidPassword :: PasswordPolicy -> Bool
isValidPassword (min, max, char, password) = checkPasswordLength >= min && checkPasswordLength <= max 
  where checkPasswordLength = length $ filter (== char) password

isValidPasswordV2 :: PasswordPolicy -> Bool
isValidPasswordV2 (check1, check2, char, password) = (password !! (check1 - 1) == char) /= (password !! (check2 - 1) == char)

parsePasswordPolicies :: String -> [PasswordPolicy]
parsePasswordPolicies s = map parse (s =~ regex)
   where
    regex :: String
    regex = "^(\\d+)-(\\d+) (\\w): (\\w+)"
    parse [_,min,max,char,pass] = (read min :: Int, read max :: Int, head char, pass) :: PasswordPolicy

main :: IO()
main = do
  input <- getContents
  let passwordPolicies = parsePasswordPolicies input
  let validPasswords = length $ filter isValidPassword passwordPolicies

  putStrLn ("Number of valid password: " ++ show validPasswords)

  let validPasswordsV2 = length $ filter isValidPasswordV2 passwordPolicies

  putStrLn ("Number of valid v2 passwords: " ++ show validPasswordsV2)


