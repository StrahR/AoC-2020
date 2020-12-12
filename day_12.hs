main = do
  input <- readFile "day_12.in"
  let input' = lines input :: [String]
  writeFile "day_12_1.out" $ show $ naloga1 input'
  writeFile "day_12_2.out" $ show $ naloga2 input'

cardinalDirs = "NESW"

move :: Char -> String -> (Int, Int) -> (Int, Int)
move 'N' n (ns, ew) = (ns + read n, ew         )
move 'S' n (ns, ew) = (ns - read n, ew         )
move 'E' n (ns, ew) = (ns         , ew + read n)
move 'W' n (ns, ew) = (ns         , ew - read n)

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (dx, dy) 0 = (dx, dy)
rotate (dx, dy) n = rotate (-dy, dx) (n - 90)

moveShip :: String -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
moveShip ('R' : n) ship     waypoint = moveWaypoint ('R' : n) waypoint ship
moveShip ('L' : n) ship     waypoint = moveWaypoint ('L' : n) waypoint ship
moveShip ('F' : n) (ns, ew) (dx, dy) = ((ns + dx*read n, ew + dy*read n), (dx, dy))
moveShip (dir : n) ship     waypoint = (move dir n ship, waypoint)

moveWaypoint :: String -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
moveWaypoint ('R' : n) waypoint ship = (ship, rotate waypoint $ read n)
moveWaypoint ('L' : n) waypoint ship = (ship, rotate waypoint $ 360 - read n)
moveWaypoint ('F' : n) waypoint ship = moveShip ('F' : n) ship waypoint
moveWaypoint (dir : n) waypoint ship = (ship, move dir n waypoint)

naloga1 =
  let aux ((ns, ew), _ ) []     = abs ns + abs ew
      aux (ship    , wp) (x:xs) = aux (moveShip x ship wp) xs
   in aux ((0, 0), (0, 1))

naloga2 =
  let aux ((ns, ew), _       ) []       = abs ns + abs ew
      aux (ship    , waypoint) (x : xs) = aux (moveWaypoint x waypoint ship) xs
   in aux ((0, 0), (1, 10))
