import           Data.Maybe (mapMaybe)
data Seat = Null | Empty | Full deriving (Eq)

instance Read Seat where
  readList []      = [([], "")]
  readList (c : k) = [((read [c] :: Seat) : p, tl) | (p, tl) <- readList k]
  readsPrec _ ('.' : k) = [(Null, k)]
  readsPrec _ ('L' : k) = [(Empty, k)]
  readsPrec _ ('#' : k) = [(Full, k)]

main = do
  input <- readFile "day_11.in"
  let input' = map read $ lines input :: [[Seat]]
  writeFile "day_11_1.out" $ show $ naloga1 input'
  writeFile "day_11_2.out" $ show $ naloga2 input'

cardinalDirs = [ (-1, -1), ( 0, -1), ( 1, -1), (-1,  0), ( 1,  0), (-1,  1), ( 0,  1), ( 1,  1) ]

(!!!) :: [a] -> Int -> Maybe a
(!!!) [] _       = Nothing
(!!!) (x : _) 0  = Just x
(!!!) (_ : xs) n = xs !!! (n-1)

getStatus :: [[Seat]] -> Int -> Int -> Maybe Seat
getStatus grid x y = grid !!! y >>= (!!! x)

neighbours :: [[Seat]] -> Int -> Int -> [Seat]
neighbours grid x y = mapMaybe (uncurry $ getStatus grid) neighbourIndexes
  where
    neighbourIndexes = map (\(di, dj) -> (x+di,y+dj)) cardinalDirs

fullNeighbours :: [[Seat]] -> Int -> Int -> Int
fullNeighbours grid x y = length $ filter (== Full) $ neighbours grid x y

nextValue :: [[Seat]] -> Int -> Int -> Seat -> Seat
nextValue grid x y Full  = if fullNeighbours grid x y >= 4 then Empty else Full
nextValue grid x y Empty = if fullNeighbours grid x y == 0 then Full else Empty
nextValue _ _ _    Null  = Null

step :: [[Seat]] -> [[Seat]]
step grid = [[nextValue grid i j x | (i, x) <- zip [0..] row]
            | (j, row) <- zip [0..] grid]

fix :: Eq t => (t -> t) -> t -> t
fix f x = let x' = f x in if x == x' then x else fix f x'

naloga1 = length . filter (== Full) . concat . fix step

fullInDirection :: [[Seat]] -> Int -> Int -> (Int, Int) -> Int
fullInDirection grid x y (dx, dy) =
  let aux grid x y (dx, dy) k =
        case getStatus grid (x+k*dx) (y+k*dy) of
          Just Full  -> 1
          Just Empty -> 0
          Just Null  -> aux grid x y (dx, dy) (k+1)
          Nothing    -> 0
   in aux grid x y (dx, dy) 1

fullNeighbours' :: [[Seat]] -> Int -> Int -> Int
fullNeighbours' grid x y = sum neighbourIndexes
  where
    neighbourIndexes = map (fullInDirection grid x y) cardinalDirs

nextValue' :: [[Seat]] -> Int -> Int -> Seat -> Seat
nextValue' grid x y Full  = if fullNeighbours' grid x y >= 5 then Empty else Full
nextValue' grid x y Empty = if fullNeighbours' grid x y == 0 then Full else Empty
nextValue' _ _ _    Null  = Null

step' :: [[Seat]] -> [[Seat]]
step' grid = [[nextValue' grid i j x | (i, x) <- zip [0..] row]
             | (j, row) <- zip [0..] grid]

naloga2 = length . filter (== Full) . concat . fix step'
