import           Data.List (sort)
main = do
  input <- readFile "day_10.in"
  let input' = map read $ lines input :: [Int]
  let input'' = sort (0:(maximum input' + 3 ) : input')
  writeFile "day_10_1.out" $ show $ naloga1 input''
  writeFile "day_10_2.out" $ show $ naloga2 input''

naloga1 =
  let aux diff1 diff3 _ [] = diff1 * diff3
      aux diff1 diff3 prev (y : ys)
          | y - prev == 1 = aux (diff1 + 1) diff3 y ys
          | y - prev == 3 = aux diff1 (diff3 + 1) y ys
          | otherwise     = aux diff1 diff3 y ys
   in aux 0 0 0

takeBlock :: (Eq a, Num a) => [a] -> [a]
takeBlock [] = []
takeBlock [x] = [x]
takeBlock (x1 : xs@(x2 : _))
  | x2 - x1 == 1 = x1 : takeBlock xs
  | otherwise    = [x1]

isValidAdapterSequence :: Int -> [Int] -> Bool
isValidAdapterSequence _ []       = True
isValidAdapterSequence n (x : xs) = (x - n <= 3) && isValidAdapterSequence x xs

blockArrangements :: Int -> Int
blockArrangements n = [1, 1, 1, 2, 4, 7, 13, 24] !! n
  -- length $ filter (isValidAdapterSequence 1) $ map (\xs -> 1 : xs ++ [n]) $ subsequences [2..n-1]

naloga2 =
  let aux acc [] = acc
      aux acc xs =
        let n = length $ takeBlock xs
         in aux (acc * blockArrangements n) (drop n xs)
   in aux 1
