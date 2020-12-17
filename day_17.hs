import           Data.HashMap.Strict            ( (!)
                                                , HashMap
                                                )
import qualified Data.HashMap.Strict           as Map
                                                ( toList
                                                , elems
                                                , fromList
                                                , lookup
                                                , lookupDefault
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( (\\) )

data State = Dead | Alive deriving (Eq)

instance Read State where
  readList []      = [([], "")]
  readList (c : k) = [ ((read [c] :: State) : p, tl) | (p, tl) <- readList k ]
  readsPrec _ ('.' : k) = [(Dead, k)]
  readsPrec _ ('#' : k) = [(Alive, k)]

instance Show State where
  showList []       s = s
  showList (c : tl) s = show c ++ showList tl s
  showsPrec _ Dead  s = '\'' : '.' : '\'' : s
  showsPrec _ Alive s = '\'' : '#' : '\'' : s
main = do
  input <- readFile "day_17.in"
  let n = 4
  let input' =
        Map.fromList $ concat
          [ [ ((x, y, 0), state) | (x, state) <- zip [-n .. n] $ read row ]
          | (y, row) <- zip [-n .. n] $ lines input
          ] :: HashMap (Int, Int, Int) State
  writeFile "day_17_1.out" $ show $ naloga1 n input'
  let input'' =
        Map.fromList $ concat
          [ [ ((x, y, 0, 0), state) | (x, state) <- zip [-n .. n] $ read row ]
          | (y, row) <- zip [-n .. n] $ lines input
          ] :: HashMap (Int, Int, Int, Int) State
  writeFile "day_17_2.out" $ show $ naloga2 n input''

cardinalDirs =
  [ (x, y, z) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1] ]
  \\ [(0, 0, 0)]

getStatus :: HashMap (Int, Int, Int) State -> (Int, Int, Int) -> Maybe State
getStatus grid loc = Map.lookup loc grid

neighbours :: HashMap (Int, Int, Int) State -> Int -> Int -> Int -> [State]
neighbours grid x y z = mapMaybe (getStatus grid) neighbourIndexes
 where
  neighbourIndexes =
    map (\(di, dj, dk) -> (x + di, y + dj, z + dk)) cardinalDirs

fullNeighbours :: HashMap (Int, Int, Int) State -> Int -> Int -> Int -> Int
fullNeighbours grid x y z = length $ filter (== Alive) $ neighbours grid x y z

nextValue
  :: HashMap (Int, Int, Int) State -> Int -> Int -> Int -> State -> State
nextValue grid x y z Alive =
  if fullNeighbours grid x y z `elem` [2, 3] then Alive else Dead
nextValue grid x y z Dead =
  if fullNeighbours grid x y z == 3 then Alive else Dead

step
  :: Int
  -> Int
  -> HashMap (Int, Int, Int) State
  -> HashMap (Int, Int, Int) State
step n k grid = Map.fromList
  [ ((i, j, k), nextValue grid i j k (Map.lookupDefault Dead (i, j, k) grid))
  | i <- [-n - k .. n + k]
  , j <- [-n - k .. n + k]
  , k <- [-k .. k]
  ]

getState :: Int -> HashMap (Int, Int, Int) State -> [((Int, Int), State)]
getState slice grid =
  [ ((i, j), v) | ((i, j, k), v) <- Map.toList grid, k == slice ]

naloga1 n grid =
  -- (\v -> map (`getState` v) [-6 .. 6])
                 length $ filter (== Alive) $ Map.elems
  (step n 6 $ step n 5 $ step n 4 $ step n 3 $ step n 2 $ step n 1 grid)


cardinalDirs4 =
  [ (x, y, z, w)
  | x <- [-1, 0, 1]
  , y <- [-1, 0, 1]
  , z <- [-1, 0, 1]
  , w <- [-1, 0, 1]
  ]
  \\ [(0, 0, 0, 0)]

getStatus'
  :: HashMap (Int, Int, Int, Int) State -> (Int, Int, Int, Int) -> Maybe State
getStatus' grid loc = Map.lookup loc grid

neighbours'
  :: HashMap (Int, Int, Int, Int) State -> Int -> Int -> Int -> Int -> [State]
neighbours' grid x y z w = mapMaybe (getStatus' grid) neighbourIndexes
 where
  neighbourIndexes =
    map (\(di, dj, dk, dl) -> (x + di, y + dj, z + dk, w + dl)) cardinalDirs4

fullNeighbours'
  :: HashMap (Int, Int, Int, Int) State -> Int -> Int -> Int -> Int -> Int
fullNeighbours' grid x y z w =
  length $ filter (== Alive) $ neighbours' grid x y z w

nextValue'
  :: HashMap (Int, Int, Int, Int) State
  -> Int
  -> Int
  -> Int
  -> Int
  -> State
  -> State
nextValue' grid x y z w Alive =
  if fullNeighbours' grid x y z w `elem` [2, 3] then Alive else Dead
nextValue' grid x y z w Dead =
  if fullNeighbours' grid x y z w == 3 then Alive else Dead

step'
  :: Int
  -> Int
  -> HashMap (Int, Int, Int, Int) State
  -> HashMap (Int, Int, Int, Int) State
step' n kk grid = Map.fromList
  [ ( (i, j, k, l)
    , nextValue' grid i j k l (Map.lookupDefault Dead (i, j, k, l) grid)
    )
  | i <- [-n - kk .. n + kk]
  , j <- [-n - kk .. n + kk]
  , k <- [-kk .. kk]
  , l <- [-kk .. kk]
  ]

getState'
  :: (Int, Int) -> HashMap (Int, Int, Int, Int) State -> [((Int, Int), State)]
getState' (z, w) grid =
  [ ((i, j), v) | ((i, j, k, l), v) <- Map.toList grid, (k, l) == (z, w) ]

naloga2 n grid =
  -- (\v -> [ (`getState'` v) (k, l) | k <- [-kk .. kk], l <- [-kk .. kk] ])
                 length $ filter (== Alive) $ Map.elems
  (step' n 6 $ step' n 5 $ step' n 4 $ step' n 3 $ step' n 2 $step' n 1 grid)
  -- where kk = 8
