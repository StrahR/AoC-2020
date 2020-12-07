-- {-# LANGUAGE TupleSections #-}

main = do
  input <- readFile "day_01.in"
  let input' = map read $ lines input :: [Int]
  -- print $ naloga1 $ explode1 input'
  -- print $ naloga2 $ explode2 input'
  writeFile "day_01_1.out" $ show $ naloga1 $ explode2 input'
  writeFile "day_01_2.out" $ show $ naloga2 $ explode3 input'

find :: (a -> Bool) -> [a] -> a
find p = head . filter p

-- naloga1 :: [(Int, Int)] -> Int
naloga1 = uncurry (*) . find (\(a, b) -> a + b == 2020)

-- naloga2 :: [(Int, Int, Int)] -> Int
naloga2 = (\(a, b, c) -> a * b * c) . find (\(a, b, c) -> a + b + c == 2020)

-- naloga1 [] = undefined
-- naloga1 ((a, b) : xs) =
--   if a + b == 2020
--     then a * b
--     else naloga1 xs

-- naloga2 :: [(Int, Int, Int)] -> Int
-- naloga2 [] = undefined
-- naloga2 ((a, b, c) : xs) =
--   if a + b + c == 2020
--     then a * b * c
--     else naloga2 xs

explode2 :: [Int] -> [(Int, Int)]
explode2 xs = [(a, b) | a <- xs, b <- xs, a >= b]

-- concatMap (\x -> map (x,) list) list

explode3 :: [Int] -> [(Int, Int, Int)]
explode3 xs = [(a, b, c) | a <- xs, b <- xs, a >= b, c <- xs, b >= c]

-- concatMap (\x -> concatMap (\y -> map (x,y,) list) list) list
