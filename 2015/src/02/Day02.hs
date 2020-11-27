module Day02
(
  calculateTotalWrappingPaperNeeded,
  calculateTotalRibbonNeeded
) where

calculateTotalWrappingPaperNeeded :: String -> Int
calculateTotalWrappingPaperNeeded input = sum $ map calculateWrappingPaperNeeded (lines input)

calculateWrappingPaperNeeded :: String -> Int
calculateWrappingPaperNeeded input = surfaceArea (l, w, h) + slack (l, w, h)
  where 
    (l, w, h) = toDimensions $ parse input

calculateTotalRibbonNeeded :: String -> Int
calculateTotalRibbonNeeded input = sum $ map calculateRibbonNeeded (lines input) 

calculateRibbonNeeded :: String -> Int
calculateRibbonNeeded input = ribbonWrap (l, w, h) + bow (l, w, h)
  where 
    (l, w, h) = toDimensions $ parse input

toDimensions :: [Int] -> (Int, Int, Int)
toDimensions [l,w,h] = (l, w, h)
toDimensions _ = error "Input should be in the format LxWxH"

parse :: String -> [Int]
parse [] = []
parse xs = (read $ takeWhile isData xs :: Int) : parse (drop 1 $ dropWhile isData xs)

isData :: Char -> Bool
isData c = c /= 'x'

surfaceArea :: (Int, Int, Int) -> Int
surfaceArea (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l

slack :: (Int, Int, Int) -> Int
slack (l, w, h) = product $ toSmallestTwo (l, w, h)

toSmallestTwo :: (Int, Int, Int) -> [Int]
toSmallestTwo (l, w, h)
  | l >= w && l >= h = [w, h]
  | h >= l && h >= w = [l, w]
  | otherwise = [h, l]

ribbonWrap :: (Int, Int, Int) -> Int
ribbonWrap (l, w, h) = foldl (\acc a -> acc + (a * 2)) 0 $ toSmallestTwo (l, w, h)

bow :: (Int, Int, Int) -> Int
bow (l, w, h) = l * w * h

showTotalWrappingPaper :: Int -> String
showTotalWrappingPaper p = "Square feet of wrapping paper " ++ show p

showTotalRibbonNeeded :: Int -> String
showTotalRibbonNeeded r = "Square feet of ribbon " ++ show r

main = do
  input <- getContents
  putStrLn $ showTotalWrappingPaper $ calculateTotalWrappingPaperNeeded input
  putStrLn $ showTotalRibbonNeeded $ calculateTotalRibbonNeeded input


