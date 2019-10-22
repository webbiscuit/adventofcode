calculateTotalWrappingPaperNeeded :: String -> Int
calculateTotalWrappingPaperNeeded input = sum $ map calculateWrappingPaperNeeded (lines input)

calculateWrappingPaperNeeded :: String -> Int
calculateWrappingPaperNeeded input = surfaceArea l w h + slack l w h
  where 
    parsed = parse input
    l = parsed !! 0
    w = parsed !! 1
    h = parsed !! 2

calculateTotalRibbonNeeded :: String -> Int
calculateTotalRibbonNeeded input = sum $ map calculateRibbonNeeded (lines input) 

calculateRibbonNeeded :: String -> Int
calculateRibbonNeeded input = ribbonWrap l w h + bow l w h
  where 
    parsed = parse input
    l = parsed !! 0
    w = parsed !! 1
    h = parsed !! 2

parse :: String -> [Int]
parse [] = []
parse xs = (read $ takeWhile isData xs :: Int) : parse (drop 1 $ dropWhile isData xs)

isData :: Char -> Bool
isData c = c /= 'x'

surfaceArea :: Int -> Int -> Int -> Int
surfaceArea l w h = 2 * l * w + 2 * w * h + 2 * h * l

slack :: Int -> Int -> Int -> Int
slack l w h = product $ toSmallestTwo l w h

toSmallestTwo :: Int -> Int -> Int -> [Int]
toSmallestTwo l w h
  | null removedMax = [l, w]  -- doesn't matter which we use
  | length removedMax == 1 = [head removedMax, maximum items]
  | otherwise = removedMax
  where 
    items = [l, w, h]
    removedMax = removeMax items

removeMax :: (Ord a) => [a] -> [a]
removeMax xs = filter (< maximum xs) xs

ribbonWrap :: Int -> Int -> Int -> Int
ribbonWrap l w h = foldl (\acc a -> acc + (a * 2)) 0 $ toSmallestTwo l w h

bow :: Int -> Int -> Int -> Int
bow l w h = l * w * h

main = do
  input <- getContents
  let wrappingPaperNeeded = show $ calculateTotalWrappingPaperNeeded input
  let wrappingPaperNeededOutput =  "Square feet of wrapping paper " ++ wrappingPaperNeeded
  putStrLn wrappingPaperNeededOutput

  let ribbonNeeded = show $ calculateTotalRibbonNeeded input
  let ribbonNeededOutput =  "Square feet of ribbon " ++ ribbonNeeded
  putStrLn ribbonNeededOutput


