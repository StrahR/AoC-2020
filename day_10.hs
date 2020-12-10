import           Data.List (sort)
main = do
  input <- readFile "day_10.in"
  let input' = map read $ lines input :: [Int]
  let input'' = sort (0:(maximum input' + 3 ) : input')
  -- print $ blockLengths input''
  writeFile "day_10_1.out" $ show $ naloga1 input''
  writeFile "day_10_2.out" $ show $ naloga2 input''

naloga1 =
  let aux diff1 diff3 _ [] = diff1 * diff3
      aux diff1 diff3 prev (y : ys)
          | y - prev == 1 = aux (diff1 + 1) diff3 y ys
          | y - prev == 3 = aux diff1 (diff3 + 1) y ys
          | otherwise     = aux diff1 diff3 y ys
   in aux 0 0 0

blockArrangements :: Int -> Int
blockArrangements n = [1, 1, 1, 2, 4, 7, 13, 24] !! n

-- reversed
blockLengths :: [Int] -> [Int]
blockLengths xs =
  let aux len lengths (x1 : xs@(x2 : _))
        | x2 - x1 == 1 = aux (len+1)        lengths  xs
        | otherwise    = aux      1  (len : lengths) xs
      aux len lengths _ = len : lengths
   in aux 1 [] xs

naloga2 = product . map blockArrangements . blockLengths
