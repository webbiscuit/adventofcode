module Day08
(
  calculateLayers,
  RawImage (RawImage),
  parseInput,
  mergeLayers 
) where

import Data.List
import Data.List.Split
import Data.Ord

data RawImage = RawImage Width Height [Pixel]
type Width = Int
type Height = Int
type Pixel = Colour
type Layer = [Pixel]
data Colour = Black | White | Transparent | ColourX deriving (Eq, Show)

calculateLayers :: RawImage -> [Layer]
calculateLayers (RawImage width height pixels) = chunksOf layerSize pixels
  where 
    layerSize = width * height

mergeLayers :: RawImage -> [Pixel]
mergeLayers image = foldl1 applyLayer (reverse $ calculateLayers image)

applyLayer :: Layer -> Layer -> Layer
applyLayer lowerLayer upperLayer = foldl (\acc p -> if snd p == Transparent then acc ++ [fst p] else acc ++ [snd p]) [] (zip lowerLayer upperLayer)
    
countPixelByColour :: Colour -> Layer -> Int
countPixelByColour colour = foldl (\acc p -> if p == colour then acc + 1 else acc) 0

calculateChecksum :: Layer -> Int
calculateChecksum layer = countPixelByColour White layer * countPixelByColour Transparent layer

parseInput :: String -> [Pixel]
parseInput = map charToColour
  where 
    charToColour '0' = Black
    charToColour '1' = White
    charToColour '2' = Transparent
    charToColour _ = ColourX

a = [Black,White,White,Black]

draw :: Width -> Height -> [Pixel] -> [String]
draw w _ pixels = chunksOf w $ map colourToChar pixels
  where
    colourToChar Black = ' '
    colourToChar White = '.'
    colourToChar _ = '?'

main = do
  input <- getLine

  let pixelsData = parseInput input
  let image = RawImage 25 6 pixelsData
  let layers = calculateLayers image

  let fewest0Layer = minimumBy (comparing (countPixelByColour Black)) layers
  let corruptionCheck = calculateChecksum fewest0Layer

  putStrLn ("Number of 1s * number of 2s on fewest 0 digit layer is " ++ show corruptionCheck)

  let drawing = draw 25 6 $ mergeLayers image
  putStrLn "Message is: "

  mapM_ putStrLn drawing
