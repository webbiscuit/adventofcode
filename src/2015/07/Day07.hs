module Day07
(
  parseStatements,
  readVariable
) where

import Data.Bits
import Data.Word
import Data.Void
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Identifier = String
type Value = Word16
type MachineState = Map.HashMap Identifier Value
type Destination = Identifier
type Source = Identifier
type Amount = Int

data Statement
  = Assign Destination Value
  | And Destination Source Source
  | ValAnd Destination Value Source
  | Or Destination Source Source
  | LShift Destination Source Amount
  | RShift Destination Source Amount
  | Not Destination Source
  deriving (Show)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser Identifier
identifier = lexeme $ many letterChar

value :: Parser Value
value = lexeme L.decimal

amount :: Parser Amount
amount = lexeme L.decimal

statement :: Parser Statement
statement = 
  try assignStatement <|> 
  try andStatement <|> 
  try valAndStatement <|>
  try orStatement <|>
  try lShiftStatement <|>
  try rShiftStatement <|>
  try notStatement

assignStatement :: Parser Statement
assignStatement = do
  value <- value
  symbol "->"
  address <- identifier
  return (Assign address value)

andStatement :: Parser Statement
andStatement = do
  value1 <- identifier
  symbol "AND"
  value2 <- identifier
  symbol "->"
  address <- identifier
  return (And address value1 value2)

valAndStatement :: Parser Statement
valAndStatement = do
  value1 <- value
  symbol "AND"
  value2 <- identifier
  symbol "->"
  address <- identifier
  return (ValAnd address value1 value2)

orStatement :: Parser Statement
orStatement = do
  value1 <- identifier
  symbol "OR"
  value2 <- identifier
  symbol "->"
  address <- identifier
  return (Or address value1 value2)

lShiftStatement :: Parser Statement
lShiftStatement = do
  source <- identifier
  symbol "LSHIFT"
  amount <- amount
  symbol "->"
  dest <- identifier
  return (LShift dest source amount)

rShiftStatement :: Parser Statement
rShiftStatement = do
  source <- identifier
  symbol "RSHIFT"
  amount <- amount
  symbol "->"
  dest <- identifier
  return (RShift dest source amount)

notStatement :: Parser Statement
notStatement = do
  symbol "NOT"
  source <- identifier
  symbol "->"
  dest <- identifier
  return (Not dest source)

initialState :: MachineState
initialState = Map.empty

finalState :: [Maybe Statement] -> MachineState
finalState = foldl (flip handleStatement) initialState

handleStatement' :: Statement -> MachineState -> MachineState
handleStatement' (Assign identifier value) state = Map.insert identifier value state
handleStatement' (And dest source1 source2) state = Map.insert dest (val1 .&. val2) state
  where val1 = fromJust $ Map.lookup source1 state
        val2 = fromJust $ Map.lookup source2 state
handleStatement' (Or dest source1 source2) state = Map.insert dest (val1 .|. val2) state
  where val1 = fromJust $ Map.lookup source1 state
        val2 = fromJust $ Map.lookup source2 state
handleStatement' (LShift dest source amount) state = Map.insert dest (shiftL val amount) state
  where val = fromJust $ Map.lookup source state
handleStatement' (RShift dest source amount) state = Map.insert dest (shiftR val amount) state
  where val = fromJust $ Map.lookup source state
handleStatement' (ValAnd dest value source) state = Map.insert dest (value .&. val) state
  where val = fromJust $ Map.lookup source state
handleStatement' (Not dest source) state = Map.insert dest (complement val) state
  where val = fromJust $ Map.lookup source state

handleStatement :: Maybe Statement -> MachineState -> MachineState
handleStatement (Just statement) = handleStatement' statement
handleStatement _ = error "Error parsing the data"

parseStatements :: [String] -> MachineState
parseStatements = finalState . map (parseMaybe statement)

readVariable :: Identifier -> MachineState  -> Maybe Value
readVariable = Map.lookup

-- main = do
--   input <- getContents
--   --finalState <- parse (tryParsing initialState) "" input
  

--   putStrLn "ERM"
--   -- finalState
--  -- putStrLn (finalState)
--   -- putStrLn $ showLightsOn $ countLightsOn input
--   -- putStrLn $ showBrightness $ measureBrightness input
