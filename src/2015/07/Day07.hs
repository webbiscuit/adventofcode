module Day07
(
  createCircuit,
  readSignal
) where
  
import Data.Bits
import Data.Word
import Data.Function.Memoize
import qualified Data.HashMap.Strict as Map
import Text.Read

type Wire = String
type Signal = Word16
type Value = Int
data Input = WireIn Wire | SignalIn Signal deriving (Show)
type SignalMap = Map.HashMap Wire Gate

data Gate = 
  Assign Input |
  And Input Input |
  Or Input Input |
  LShift Input Value |
  RShift Input Value |
  Not Input

instance Show Gate where 
  show (Assign a) = mconcat[getVal a, " -> "]
  show (And a b) = mconcat[getVal a," AND ", getVal b]
  show (Or a b)  = mconcat[getVal a," OR ", getVal b]
  show (LShift a b)  = mconcat[getVal a," LSHIFT ", show b]
  show (RShift a b)  = mconcat[getVal a," RSHIFT ", show b]
  show (Not a)  = mconcat["NOT ", show a]
      
getVal (WireIn w) = w
getVal (SignalIn v) = show v

addGates :: [(Wire, Gate)] -> SignalMap 
addGates = Map.fromList

parse :: [String] -> (Wire, Gate)
parse [          x, "->", w] = (w, Assign (addInput x))
parse [x, "AND", y, "->", w] = (w, And (addInput x) (addInput y))
parse [x, "OR",  y, "->", w] = (w, Or (addInput x) (addInput y))
parse [x, "LSHIFT",  y, "->", w] = (w, LShift (addInput x) (addValue y))
parse [x, "RSHIFT",  y, "->", w] = (w, RShift (addInput x) (addValue y))
parse [      "NOT",  x, "->", w] = (w, Not (addInput x))

-- Find if it's an Int or String
addInput :: String -> Input
addInput s = case readMaybe s of
  Nothing -> WireIn s
  Just n -> SignalIn n
  
addValue :: String -> Value
addValue s = read s :: Value

parseStatement :: String -> (Wire, Gate)
parseStatement = parse . words

parseStatements :: String-> [(Wire, Gate)]
parseStatements = map parseStatement . lines

eval :: SignalMap -> Wire -> Signal
eval m = me
  where 
    e :: Wire -> Signal
    e gate = case m Map.! gate of
      (Assign a) -> getVal a
      (And a b) -> getVal a .&. getVal b
      (Or a b)  -> getVal a .|. getVal b
      (LShift a b)  -> shiftL (getVal a) b
      (RShift a b)  -> shiftR (getVal a) b
      (Not a)  -> complement (getVal a)
    me = memoize e
    getVal (WireIn w) = me w
    getVal (SignalIn s) = s

showWire :: Wire -> Signal -> String
showWire wire value = concat["Wire ", wire, " is ", show value]

readSignal :: SignalMap -> Wire -> Signal
readSignal = eval

setSignal :: SignalMap -> Wire -> Signal -> SignalMap
setSignal m w v = Map.insert w (Assign (SignalIn v)) m

createCircuit :: String -> SignalMap
createCircuit = addGates . parseStatements

main = do
  input <- getContents
  let circuit = createCircuit input
  let a = readSignal circuit "a"
  putStrLn $ showWire "a" a
  putStrLn "Tinkering..."
  let circuit2 = setSignal circuit "b" a
  let a2 = readSignal circuit2 "a"
  putStrLn $ showWire "a" a2
