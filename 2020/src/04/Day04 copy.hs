module Day04
(
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

newtype EyeColour = EyeColour String deriving(Show)
newtype BirthYear = BirthYear Integer deriving(Show)

data Passport = MkPassport {
  eyeColour :: EyeColour,
  birthYear :: BirthYear
} deriving (Show)

lexer = P.makeTokenParser emptyDef

symbol = P.symbol lexer
colon = P.colon lexer
stringLiteral = P.stringLiteral lexer
integer = P.integer lexer
whiteSpace = P.whiteSpace lexer
-- eol = P.eol lexer

-- passportBreak = do
--   newline
--   newline


-- parseX :: String -> String
-- doParse :: String -> Char
doParse = runParserX mainParser

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

birthYearParser = do
  ls <- symbol "byr"
  colon
  v <- integer
  return (BirthYear v)

-- issueYear = do
--   ls <- symbol "iyr"
--   colon
--   v <- value
--   return ls

eyeColourParser = do
  ls <- symbol "ecl"
  colon
  v <- many alphaNum
  return (EyeColour v)

passportsParser = do
  px <- many passportParser
  eof
  return px

passportParser = do
  attrs <- manyTill kvParser newline -- (noneOf " ")
  -- symbol " " <|> whiteSpace
  whiteSpace
  -- passportBreak
  return attrs
  -- ecl <- eyeColourParser
  -- whiteSpace
  -- byr <- birthYearParser
  -- -- passportBreak 
  -- return (MkPassport ecl byr) 

kvParser = do
  key <- manyTill anyChar (symbol " ")
  colon
  value <- manyTill anyChar (symbol " ")
  return (key, value)

--showParser :: Parser String
mainParser = passportsParser <?> "Parse Error"

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
