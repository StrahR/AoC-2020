main = do
  input <- readFile "day_01.in"
  let input' = map read $ lines input :: [Int]
  writeFile "day_01_1.out" $ show $ naloga1 input'
  writeFile "day_01_2.out" $ show $ naloga2 input'

naloga1 xs = head [a * b | a <- xs, b <- xs, a >= b, a + b == 2020]

naloga2 xs = head [a * b * c | a <- xs, b <- xs, a >= b, c <- xs, b >= c, a + b + c == 2020]
