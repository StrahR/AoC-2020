main = do
  input <- readFile "day_03.in"
  let input' = lines input
  -- print $ naloga2 input'
  -- naloga2 input'
  writeFile "day_03_1.out" $ show $ naloga1 input'
  writeFile "day_03_2.out" $ show $ naloga2 input'

atIndex :: Int -> String -> Char
atIndex 0 s        = head s
atIndex n (_ : ss) = atIndex (n - 1) ss

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

isTree :: Int -> String -> Bool
isTree n s = '#' == atIndex (n `mod` length s) s

-- filter' :: Int -> Int -> (Int -> String -> Bool) -> [String] -> [String]
-- filter' k m p =
--   let aux n [] = []
--       aux n (x : xs) | n `mod` m /= 0 = aux (n + 1) xs
--       aux n (x : xs) =
--         if p (k * quot n m) x
--           then x : aux (n + 1) xs
--           else aux (n + 1) xs
--    in aux 0

diagonal :: Int -> Int -> [String] -> String
diagonal k m =
  let aux _ [] = ""
      aux n (_ : xs) | n `mod` m /= 0 = aux (n + 1) xs
      aux n (x : xs) =
        let i = (k * quot n m) `mod` length x
         in atIndex i x : aux (n + 1) xs
   in aux 0

toboggan :: Int -> Int -> [String] -> Int
-- toboggan k m = length . filter' k m isTree
toboggan k m = count '#' . diagonal k m

naloga1 = toboggan 3 1

-- naloga2 = product . mapM (uncurry toboggan) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

naloga2 forest =
  1
    * toboggan 1 1 forest
    * toboggan 3 1 forest
    * toboggan 5 1 forest
    * toboggan 7 1 forest
    * toboggan 1 2 forest
