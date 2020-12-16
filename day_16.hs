import Control.Applicative (Alternative ((<|>)))
import Data.Char
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map (empty, insert)
import Data.List (intersect, sortOn, transpose)
import Data.List.Split
import qualified Data.Set as Set (empty, insert, notMember)
import Text.ParserCombinators.ReadP
  ( eof,
    many1,
    readP_to_S,
    satisfy,
    string,
  )

main = do
  input <- splitOn "\n\n" <$> readFile "day_16.in"
  -- print $ parseConditions "class: 1-3 or 5-7"
  let conditions = map parseConditions $ lines (head input) :: [(String, Int, Int, Int, Int)]
  -- print conditions
  let myTicket = map read $ splitOn "," $ lines (input !! 1) !! 1 :: [Int]
  -- print myTicket
  let nearbyTickets = map (map read . splitOn ",") $ tail $ lines (input !! 2) :: [[Int]]
  -- print nearbyTickets
  writeFile "day_16_1.out" $ show $ naloga1 conditions myTicket nearbyTickets
  let nearbyTickets' = filter (valid conditions) nearbyTickets
  -- print (map (!! 14) nearbyTickets')
  -- naloga2 conditions myTicket nearbyTickets'
  writeFile "day_16_2.out" $ show $ naloga2 conditions myTicket nearbyTickets'

-- print $ map (candidates conditions) nearbyTickets'

name = many1 (satisfy isAlpha <|> satisfy isSpace)

space = satisfy isSpace

number = many1 (satisfy isDigit)

condition = do
  name <- name
  string ": "
  min1 <- number
  string "-"
  max1 <- number
  string " or "
  min2 <- number
  string "-"
  max2 <- number
  eof
  return (name, read min1, read max1, read min2, read max2)

parseConditions :: String -> ([Char], Int, Int, Int, Int)
parseConditions = fst . head . readP_to_S condition

intersection [x] = x
intersection (x1 : x2 : xs) = intersection (intersect x1 x2 : xs)

invalidity :: [(String, Int, Int, Int, Int)] -> [Int] -> Int
invalidity conditions ticket =
  sum $
    intersection
      [ filter (\k -> k < min1 || k > max1 && k < min2 || k > max2) ticket
        | (_, min1, max1, min2, max2) <- conditions
      ]

naloga1 conditions _ tickets = sum $ map (invalidity conditions) tickets

valid conditions ticket =
  null $ intersection [filter (\k -> k < min1 || k > max1 && k < min2 || k > max2) ticket | (_, min1, max1, min2, max2) <- conditions]

candidates :: [(String, Int, Int, Int, Int)] -> [Int] -> [[(String, Int)]]
candidates conditions ticket =
  map (map fst) $
    [ filter (\(_, k) -> not (k < min1 || k > max1 && k < min2 || k > max2)) (zip (zip (repeat name) [0 ..]) ticket)
      | (name, min1, max1, min2, max2) <- conditions
    ]

getPossiblePositions :: [(String, Int, Int, Int, Int)] -> [[Int]] -> [[(String, Int)]]
getPossiblePositions conditions tickets = map intersection $ transpose $ map (candidates conditions) tickets

-- buildMap :: [[(String, Int)]] -> IO (HashMap String Int)
buildMap :: [[(String, Int)]] -> HashMap String Int
buildMap =
  let aux map _ [] = map
      aux map assigned (x : xs) =
        -- print x
        -- print assigned
        -- print $ filter (\t -> snd t `Set.notMember` assigned) x
        let (name, number) = head $ filter (\t -> snd t `Set.notMember` assigned) x
         in aux (Map.insert name number map) (Set.insert number assigned) xs
   in aux Map.empty Set.empty

naloga2 conditions ticket tickets =
  let assignments = buildMap $ sortOn length $ getPossiblePositions conditions tickets
   in product $
        map
          (\t -> ticket !! (assignments ! t))
          [ "departure location",
            "departure station",
            "departure platform",
            "departure track",
            "departure date",
            "departure time"
          ]

-- naloga2 _ ticket _ = (ticket !! 10) * (ticket !! 13) * (ticket !! 8) * (ticket !! 18) * (ticket !! 5) * (ticket !! 16)
