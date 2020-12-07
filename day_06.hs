import           Data.List
import           Data.List.Split (splitOn)

main = do
  input <- readFile "day_06.in"
  let input' = map lines $ splitOn "\n\n" input :: [[String]]
  writeFile "day_06_1.out" $ show $ naloga1 input'
  writeFile "day_06_2.out" $ show $ naloga2 input'

naloga1 = sum . map (length . foldl1' union)

naloga2 = sum . map (length . foldl1' intersect)
