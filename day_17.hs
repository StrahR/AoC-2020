import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map (elems, fromList, lookup, lookupDefault)
import Data.List ((\\))
import Data.Maybe (mapMaybe)

data State = Dead | Alive deriving (Eq)

instance Read State where
  readsPrec _ ('.' : k) = [(Dead, k)]
  readsPrec _ ('#' : k) = [(Alive, k)]

main = do
  input <- readFile "day_17.in"
  let n = 4
  let input' =
        Map.fromList $
          concat
            [ [((x, y, 0), read [state]) | (x, state) <- zip [- n .. n] row]
              | (y, row) <- zip [- n .. n] $ lines input
            ] ::
          HashMap (Int, Int, Int) State
  writeFile "day_17_1.out" $ show $ naloga1 n input'
  let input'' =
        Map.fromList $
          concat
            [ [((x, y, 0, 0), read [state]) | (x, state) <- zip [- n .. n] row]
              | (y, row) <- zip [- n .. n] $ lines input
            ] ::
          HashMap (Int, Int, Int, Int) State
  writeFile "day_17_2.out" $ show $ naloga2 n input''

cart2 xs = [(x, y) | x <- xs, y <- xs]

cart3 xs = [(x, y, z) | (x, y) <- cart2 xs, z <- xs]

cart4 xs = [(x, y, z, w) | (x, y, z) <- cart3 xs, w <- xs]

cardinalDirs = cart3 [-1, 0, 1] \\ [(0, 0, 0)]

getStatus :: HashMap (Int, Int, Int) State -> (Int, Int, Int) -> Maybe State
getStatus grid loc = Map.lookup loc grid

neighbours :: HashMap (Int, Int, Int) State -> Int -> Int -> Int -> [State]
neighbours grid x y z = mapMaybe (getStatus grid) neighbourIndexes
  where
    neighbourIndexes = map (\(di, dj, dk) -> (x + di, y + dj, z + dk)) cardinalDirs

fullNeighbours :: HashMap (Int, Int, Int) State -> Int -> Int -> Int -> Int
fullNeighbours grid x y z = length $ filter (== Alive) $ neighbours grid x y z

nextValue :: HashMap (Int, Int, Int) State -> Int -> Int -> Int -> State -> State
nextValue grid x y z Alive = if fullNeighbours grid x y z `elem` [2, 3] then Alive else Dead
nextValue grid x y z Dead = if fullNeighbours grid x y z == 3 then Alive else Dead

step :: Int -> Int -> HashMap (Int, Int, Int) State -> HashMap (Int, Int, Int) State
step n kk grid =
  Map.fromList
    [ ((i, j, k), nextValue grid i j k (Map.lookupDefault Dead (i, j, k) grid))
      | (i, j) <- cart2 [- n - kk .. n + kk],
        k <- [- kk .. kk]
    ]

loop :: (Int -> b -> b) -> Int -> b -> b
loop f 1 = f 1
loop f n = f n . loop f (n - 1)

naloga1 :: Int -> HashMap (Int, Int, Int) State -> Int
naloga1 n grid = length $ filter (== Alive) finalState
  where
    finalState = Map.elems (loop (step n) 6 grid)

cardinalDirs4 = cart4 [-1, 0, 1] \\ [(0, 0, 0, 0)]

getStatus' :: HashMap (Int, Int, Int, Int) State -> (Int, Int, Int, Int) -> Maybe State
getStatus' grid loc = Map.lookup loc grid

neighbours' :: HashMap (Int, Int, Int, Int) State -> Int -> Int -> Int -> Int -> [State]
neighbours' grid x y z w = mapMaybe (getStatus' grid) neighbourIndexes
  where
    neighbourIndexes = map (\(di, dj, dk, dl) -> (x + di, y + dj, z + dk, w + dl)) cardinalDirs4

fullNeighbours' :: HashMap (Int, Int, Int, Int) State -> Int -> Int -> Int -> Int -> Int
fullNeighbours' grid x y z w = length $ filter (== Alive) $ neighbours' grid x y z w

nextValue' :: HashMap (Int, Int, Int, Int) State -> Int -> Int -> Int -> Int -> State -> State
nextValue' grid x y z w Alive = if fullNeighbours' grid x y z w `elem` [2, 3] then Alive else Dead
nextValue' grid x y z w Dead = if fullNeighbours' grid x y z w == 3 then Alive else Dead

step' :: Int -> Int -> HashMap (Int, Int, Int, Int) State -> HashMap (Int, Int, Int, Int) State
step' n kk grid =
  Map.fromList
    [ ((i, j, k, l), nextValue' grid i j k l (Map.lookupDefault Dead (i, j, k, l) grid))
      | (i, j) <- cart2 [- n - kk .. n + kk],
        (k, l) <- cart2 [- kk .. kk]
    ]

naloga2 n grid = length $ filter (== Alive) finalState
  where
    finalState = Map.elems (loop (step' n) 6 grid)
