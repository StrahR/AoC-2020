main = do
  input <- readFile "day_09.in"
  let n = 25
  let input' = map read $ lines input :: [Int]
  writeFile "day_09_1.out" $ show $ naloga1 input' n
  writeFile "day_09_2.out" $ show $ naloga2 input' n

isSum :: Int -> [Int] -> Bool
isSum n xs = not $ null [(a,b) | a <- xs, b <- xs, a + b == n]

naloga1 xs n =
  let aux acc (x : xs)
        | length xs < n       = acc
        | isSum x (take n xs) = aux acc xs
        | otherwise           = aux (x : acc) xs
   in head $ aux [] (reverse xs)

naloga2 xs n =
  let k = naloga1 xs n
      aux runningSum len nums@(h : tl) (x : xs)
        | runningSum + x < k = aux (runningSum + x) (len + 1) nums xs
        | runningSum + x > k = aux (runningSum - h) (len - 1) tl (x : xs)
        | otherwise          = minimum (take len nums) + maximum (take len nums)
   in aux 0 0 xs xs

