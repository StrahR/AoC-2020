import           Data.Maybe (mapMaybe)
data Seat = Null | Empty | Full
  deriving Eq

main = do
  input <- readFile "day_11.in"
  let input' = map (map toSeat) $ lines input :: [[Seat]]
  -- print $ fullNeighbours' input' 3 4
  -- print $ fullNeighbours' input' 1 1
  -- print $ fullNeighbours' input' 3 3
  -- print $ (map . map) (\(i, j, x) -> (i, j, fromSeat x)) $ enumerate $ step input'
  -- putStrLn . unlines $ (map . map) fromSeat input'
  -- putStrLn . unlines $ (map . map) fromSeat $ step input'
  -- putStrLn . unlines $ (map . map) fromSeat $ step $ step input'
  -- putStrLn . unlines $ (map . map) fromSeat $ fix step input'
  writeFile "day_11_1.out" $ show $ naloga1 input'
  writeFile "day_11_2.out" $ show $ naloga2 input'

toSeat :: Char -> Seat
toSeat '.' = Null
toSeat 'L' = Empty
toSeat '#' = Full

fromSeat :: Seat -> Char
fromSeat Null  = '.'
fromSeat Empty = 'L'
fromSeat Full  = '#'

getStatus :: [[Seat]] -> Int -> Int -> Maybe Seat
getStatus grid x y =
  if x < 0 || y < 0 || x >= length (head grid) || y >= length grid
    then Nothing
    else Just (grid !! y !! x)

neighbours :: [[Seat]] -> Int -> Int -> [Seat]
neighbours grid x y = mapMaybe (uncurry $ getStatus grid) neighbourIndexes
  where
    neighbourIndexes = [
        (x-1, y-1),
        (x  , y-1),
        (x+1, y-1),
        (x-1, y  ),
        (x+1, y  ),
        (x-1, y+1),
        (x  , y+1),
        (x+1, y+1)
      ]

fullNeighbours :: [[Seat]] -> Int -> Int -> Int
fullNeighbours grid x y = length $ filter (== Full) $ neighbours grid x y

nextValue :: [[Seat]] -> Int -> Int -> Seat -> Seat
nextValue grid x y Full  = if fullNeighbours grid x y >= 4 then Empty else Full
nextValue grid x y Empty = if fullNeighbours grid x y == 0 then Full else Empty
nextValue _ _ _    Null  = Null

step :: [[Seat]] -> [[Seat]]
step grid =
  [[nextValue grid i j x | (i, x)   <- zip [0..] row]
                         | (j, row) <- zip [0..] grid]

fix :: Eq t => (t -> t) -> t -> t
fix f x = let x' = f x in if x == x' then x else fix f x'

naloga1 = length . filter (== Full) . concat . fix step

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
    neighbourIndexes = [
        fullInDirection grid x y (-1, -1),
        fullInDirection grid x y ( 0, -1),
        fullInDirection grid x y ( 1, -1),
        fullInDirection grid x y (-1,  0),
        fullInDirection grid x y ( 1,  0),
        fullInDirection grid x y (-1,  1),
        fullInDirection grid x y ( 0,  1),
        fullInDirection grid x y ( 1,  1)
      ]

nextValue' :: [[Seat]] -> Int -> Int -> Seat -> Seat
nextValue' grid x y Full  = if fullNeighbours' grid x y >= 5 then Empty else Full
nextValue' grid x y Empty = if fullNeighbours' grid x y == 0 then Full else Empty
nextValue' _ _ _    Null  = Null

step' :: [[Seat]] -> [[Seat]]
step' grid =
  [[nextValue' grid i j x | (i, x)   <- zip [0..] row]
                          | (j, row) <- zip [0..] grid]

naloga2 = length . filter (== Full) . concat . fix step'
