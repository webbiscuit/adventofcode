module Day07
(
  execute,
  read
) where

import Data.Bits
import Data.Word
import Data.Void
import qualified Data.HashMap.Strict as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Address = String
type Value = Word16
type MachineState = Map.HashMap Address Value

data Op
  = And
  | Or
  | Set Address Value
  deriving (Eq, Show)

pValue :: Parser Value
pValue = L.decimal

pAddress :: Parser Address
pAddress = some letterChar

arrow :: Parser String
arrow = string " -> "

unaryOpP :: MachineState -> Parser MachineState
unaryOpP state = do
  val <- pValue
  _ <- arrow
  address <- pAddress
  _ <- eof
  return $ execute (Set address val) state

-- binaryOpP = do
--   val1 <- many letterChar -- (:) <$> letterChar <*> many alphaNumChar
--   op <- binaryOpChoices
--   val2 <- many letterChar
--   _ <- arrow
--   dest <- many letterChar
--   _ <- eof
--   return $ binaryExecute op val1 val2 dest

-- binaryOpChoices :: Parser Op
-- binaryOpChoices = choice 
--   [
--     And <$ string " AND ",
--     Or <$ string " OR "
--   ]

-- binaryExecute And a b c = "This is and"
-- binaryExecute Or a b c = "This is or"

-- unaryExecute :: Op -> Expr -> Expr -> String
-- unaryExecute Set a b = "This is a set " ++ a

initialState :: MachineState
initialState = Map.empty

execute :: Op -> MachineState -> MachineState
execute (Set address value) = Map.insert address value

tryParsing :: MachineState -> Parser MachineState
tryParsing = unaryOpP  -- <|> binaryOpP 

-- parse :: 
--doParseWork :: MachineState -> String
doParseWork st = parse (tryParsing st) ""

ex = ["1 -> a", "2 -> b"]


printResults input = case parse (tryParsing initialState) "" input of
  Left bundle -> putStrLn (errorBundlePretty bundle)
  Right xs -> print (xs)

main = do
  input <- getContents
  --finalState <- parse (tryParsing initialState) "" input
  

  putStrLn "ERM"
  -- finalState
 -- putStrLn (finalState)
  -- putStrLn $ showLightsOn $ countLightsOn input
  -- putStrLn $ showBrightness $ measureBrightness input
