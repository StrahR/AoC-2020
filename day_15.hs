import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map (empty, insert, lookup)
import           Data.List.Split

main = do
  input <- splitOn "," <$> readFile "day_15.in"
  let input' = map read input :: [Int]
  writeFile "day_15_1.out" $ show $ naloga1 input'
  writeFile "day_15_2.out" $ show $ naloga2 input'

findnth stop input =
  let initMap acc _ []       = acc
      initMap acc n (x : xs) = initMap (Map.insert x n acc) (n+1) xs
      aux _ n k | n == stop - 1  = k
      aux map n k  =
        case Map.lookup k map of
          Just m  -> aux (Map.insert k n map) (n+1) (n - m)
          Nothing -> aux (Map.insert k n map) (n+1) 0
   in aux (initMap Map.empty 0 input) (length input) 0

naloga1 = findnth 2020

naloga2 = findnth 30000000
