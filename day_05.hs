import Data.List

main = do
  input <- readFile "day_05.in"
  let input' = lines input
  -- print $ naloga2 input
  -- naloga2 input
  writeFile "day_05_1.out" $ show $ naloga1 input'
  writeFile "day_05_2.out" $ show $ naloga2 input'

toNum :: String -> Int
toNum =
    let
        aux "" = 0
        aux ('F': cs) = 0 + 2 * aux cs
        aux ('L': cs) = 0 + 2 * aux cs
        aux ('B': cs) = 1 + 2 * aux cs
        aux ('R': cs) = 1 + 2 * aux cs
     in aux . reverse

naloga1 = foldr (max . toNum) 0

naloga2 tickets =
    let tickets'@(h:_) = sort $ map toNum tickets
     in filter (\(x, y) -> x - y /= 1) $ zip tickets' (h-1 : tickets')