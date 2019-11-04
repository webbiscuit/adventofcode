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
  | Or Destination Source Source
  | LShift Destination Source Amount
  | RShift Destination Source Amount
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
  try orStatement <|>
  try lShiftStatement <|>
  try rShiftStatement 

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

-- parseStatement = 

-- type MachineState = Map.HashMap Address Value

-- data Op
--   = And
--   | Or
--   | Set Address Value
--   deriving (Eq, Show)

-- pValue :: Parser Value
-- pValue = L.decimal

-- pAddress :: Parser Address
-- pAddress = some letterChar

-- arrow :: Parser String
-- arrow = string " -> "

-- unaryOpP :: MachineState -> Parser MachineState
-- unaryOpP state = do
--   val <- pValue
--   _ <- arrow
--   address <- pAddress
--   _ <- eof
--   return $ execute (Set address val) state

-- -- binaryOpP = do
-- --   val1 <- many letterChar -- (:) <$> letterChar <*> many alphaNumChar
-- --   op <- binaryOpChoices
-- --   val2 <- many letterChar
-- --   _ <- arrow
-- --   dest <- many letterChar
-- --   _ <- eof
-- --   return $ binaryExecute op val1 val2 dest

-- -- binaryOpChoices :: Parser Op
-- -- binaryOpChoices = choice 
-- --   [
-- --     And <$ string " AND ",
-- --     Or <$ string " OR "
-- --   ]

-- -- binaryExecute And a b c = "This is and"
-- -- binaryExecute Or a b c = "This is or"

-- -- unaryExecute :: Op -> Expr -> Expr -> String
-- -- unaryExecute Set a b = "This is a set " ++ a

initialState :: MachineState
initialState = Map.empty

-- execute :: Op -> MachineState -> MachineState
-- execute (Set address value) = Map.insert address value

-- tryParsing :: MachineState -> Parser MachineState
-- tryParsing = unaryOpP  -- <|> binaryOpP 

-- -- parse :: 
-- --doParseWork :: MachineState -> String
-- doParseWork st = parse (tryParsing st) ""

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
