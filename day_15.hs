import qualified Data.IntMap.Lazy as Map (fromList, insert, lookup)
import           Data.List.Split

main = do
  input <- splitOn "," <$> readFile "day_15.in"
  let input' = map read input :: [Int]
  writeFile "day_15_1.out" $ show $ naloga1 input'
  writeFile "day_15_2.out" $ show $ naloga2 input'

findnth :: Int -> [Int] -> Int
findnth stop input =
  let aux _ n k | n == stop - 1  = k
      aux map n k  =
        case Map.lookup k map of
          Just m  -> aux (Map.insert k n map) (n+1) (n - m)
          Nothing -> aux (Map.insert k n map) (n+1) 0
   in aux (Map.fromList $ zip input [0..]) (length input) 0

naloga1 = findnth 2020

naloga2 = findnth 30000000
