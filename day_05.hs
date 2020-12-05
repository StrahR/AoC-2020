import Data.List

main = do
  input <- readFile "day_05.in"
  let input' = lines input
  writeFile "day_05_1.out" $ show $ naloga1 input'
  writeFile "day_05_2.out" $ show $ naloga2 input'

toNum :: String -> Int
toNum =
  let aux [] = 0
      aux (x : xs) = x + 2 * aux xs
      bitVal 'F' = 0
      bitVal 'L' = 0
      bitVal 'B' = 1
      bitVal 'R' = 1
   in aux . reverse . map bitVal

naloga1 = maximum . map toNum

naloga2 tickets =
  let tickets'  = sort $ map toNum tickets
      tickets'' = zip (tail tickets') tickets'
      [(a, b)]  = filter (\(x, y) -> x - y /= 1) tickets''
   in quot (a + b) 2
