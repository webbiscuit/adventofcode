module Day04
(
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

newtype EyeColour = EyeColour String deriving(Show)

lexer = P.makeTokenParser emptyDef

symbol = P.symbol lexer
colon = P.colon lexer
stringLiteral = P.stringLiteral lexer




-- parseX :: String -> String
-- doParse :: String -> Char
doParse = runParserX showParser

-- newline :: Parser Char
-- newline = char '\n'

-- listPassports = do
--   ls <- newline -- <|> eof
--   return ls

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)

-- birthYear = do
--   ls <- symbol "byr"
--   colon
--   v <- text
--   return ls

-- issueYear = do
--   ls <- symbol "iyr"
--   colon
--   v <- value
--   return ls

eyeColour = do
  ls <- symbol "ecl"
  colon
  v <- many alphaNum
  return (EyeColour v)

--showParser :: Parser String
showParser = eyeColour <?> "Parse Error"

runParserX :: Parser a -> String -> a
runParserX p str = case parse p "" str of
  Left err -> error $ "parse error at " ++ show err
  Right val -> val

ex = readFile "super_simple.txt"

main :: IO ()
main = do
  input <- getContents

  let treesHit = 42

  putStrLn $ "Number of trees hit is " ++ show treesHit
