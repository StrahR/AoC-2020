main = do
  input <- readFile "day_12.in"
  let input' = lines input :: [String]
  writeFile "day_12_1.out" $ show $ naloga1 input'
  writeFile "day_12_2.out" $ show $ naloga2 input'

cardinalDirs = "NESW"

moveShip ('N' : n) dir          (ns, ew) = ((ns + read n, ew         ), dir)
moveShip ('S' : n) dir          (ns, ew) = ((ns - read n, ew         ), dir)
moveShip ('E' : n) dir          (ns, ew) = ((ns         , ew + read n), dir)
moveShip ('W' : n) dir          (ns, ew) = ((ns         , ew - read n), dir)
moveShip ('R' : n) dir          (ns, ew) = ((ns, ew), rotate dir $ read n)
moveShip ('L' : n) dir          (ns, ew) = ((ns, ew), rotate dir $ 360 - read n)
moveShip ('F' : n) dir@(dx, dy) (ns, ew) = ((ns + dx*read n, ew + dy*read n), dir)

rotate (dx, dy) 0 = (dx, dy)
rotate (dx, dy) n = rotate (-dy, dx) (n - 90)

naloga1 =
  let aux ((ns, ew), _  ) []    = abs ns + abs ew
      aux (ship    , wp) (x:xs) = aux (moveShip x wp ship) xs
   in aux ((0, 0), (0, 1))

moveWaypoint ('N' : n) (ns, ew) ship     = ((ns + read n, ew         ), ship)
moveWaypoint ('S' : n) (ns, ew) ship     = ((ns - read n, ew         ), ship)
moveWaypoint ('E' : n) (ns, ew) ship     = ((ns         , ew + read n), ship)
moveWaypoint ('W' : n) (ns, ew) ship     = ((ns         , ew - read n), ship)
moveWaypoint ('R' : n) waypoint ship     = (rotate waypoint $ read n      , ship)
moveWaypoint ('L' : n) waypoint ship     = (rotate waypoint $ 360 - read n, ship)
moveWaypoint ('F' : n) (dx, dy) (ns, ew) = ((dx, dy), (ns + dx*read n, ew + dy*read n))

naloga2 =
  let aux (_, (ns, ew)) []          = abs ns + abs ew
      aux (waypoint, ship) (x : xs) = aux (moveWaypoint x waypoint ship) xs
   in aux ((1, 10), (0, 0))
