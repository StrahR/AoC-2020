import           Data.List.Split (splitOn)

main = do
  input <- readFile "day_02.in"
  let input' = lines input
  -- print $ isValidPwd2 "1-3 c: cca"
  -- print $ ('c' == atIndex 1 "cca") /= ('c' == atIndex 3 "cca")
  -- print $ naloga2 input'
  writeFile "day_02_1.out" $ show $ naloga1 input'
  writeFile "day_02_2.out" $ show $ naloga2 input'

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

isValidPwd1 :: String -> Bool
isValidPwd1 s =
  min <= k && k <= max
  where
    [minmax, [c, ':'], pwd] = splitOn " " s
    [min, max] = map read $ splitOn "-" minmax :: [Int]
    k = count c pwd

naloga1 = length . filter isValidPwd1

atIndex :: Int -> String -> Char
atIndex 1 s        = head s
atIndex n (_ : ss) = atIndex (n - 1) ss

isValidPwd2 :: String -> Bool
isValidPwd2 s =
  (c == atIndex min pwd) /= (c == atIndex max pwd)
  where
    [minmax, [c, ':'], pwd] = splitOn " " s
    [min, max] = map read $ splitOn "-" minmax :: [Int]

naloga2 = length . filter isValidPwd2
