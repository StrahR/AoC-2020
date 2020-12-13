import           Data.List.Split

main = do
  input <- lines . map (\c -> if c == 'x' then '0' else c) <$> readFile "day_13.in"
  let n = read $ head input
  let input' = filter (/= 0) $ map read $ splitOn "," $ input !! 1
  let input'' = filter (\(_, t) -> t /= 0) $ zip [0..] $ map read $ splitOn "," $ input !! 1
  writeFile "day_13_1.out" $ show $ naloga1 n input'
  writeFile "day_13_2.out" $ show $ naloga2 input''

naloga1 n = uncurry (*) . minimum . map (\m -> (m - n `mod` m, m))

bezout a b =
  let aux 0 _ _ s' _ t'  = (s', t')
      aux r r' s s' t t' = aux (r' - q*r) r (s' - q*s) s (t' - q*t) t where q = quot r' r
   in aux b a 0 1 1 0

crt' (a1, n1) (a2, n2) = (a1*m2*n2 + a2*m1*n1) `mod` (n1*n2) where (m1, m2) = bezout n1 n2

crt [e1, e2]       = snd e1 * snd e2 - crt' e1 e2
crt (e1 : e2 : xs) = crt ((crt' e1 e2, snd e1 * snd e2) : xs)

naloga2 = crt
