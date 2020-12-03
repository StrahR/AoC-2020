import Data.List.Split

main = do
  input <- readFile "day_03.in"
  let input' = lines input
  -- print $ naloga2 input'
  -- naloga2 input'
  writeFile "day_03_1.out" $ show $ naloga1 input'
  writeFile "day_03_2.out" $ show $ naloga2 input'

atIndex :: Int -> String -> Char
atIndex 0 s = head s
atIndex n (s : ss) = atIndex (n - 1) ss

isTree :: Int -> String -> Bool
isTree n s = '#' == atIndex (n `mod` length s) s

filter' :: Int -> Int -> (Int -> String -> Bool) -> [String] -> [String]
filter' k m p =
  let aux n [] = []
      aux n (x : xs) | n `mod` m /= 0 = aux (n + 1) xs
      aux n (x : xs) =
        if p (k * quot n m) x
          then x : aux (n + 1) xs
          else aux (n + 1) xs
   in aux 0

toboggan k m = length . filter' k m isTree

naloga1 = toboggan 3 1

naloga2 xs =
  toboggan 1 1 xs
    * toboggan 3 1 xs
    * toboggan 5 1 xs
    * toboggan 7 1 xs
    * toboggan 1 2 xs