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
  let conditions = map parseConditions $ lines (head input) :: [(String, Int, Int, Int, Int)]
  let myTicket = map read $ splitOn "," $ lines (input !! 1) !! 1 :: [Int]
  let nearbyTickets = map (map read . splitOn ",") $ tail $ lines (input !! 2) :: [[Int]]
  writeFile "day_16_1.out" $ show $ naloga1 conditions myTicket nearbyTickets
  let nearbyTickets' = filter (null . invalid conditions) nearbyTickets
  writeFile "day_16_2.out" $ show $ naloga2 conditions myTicket nearbyTickets'

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

invalid :: [(String, Int, Int, Int, Int)] -> [Int] -> [Int]
invalid conditions ticket =
  intersection [filter (\k -> k < min1 || k > max1 && k < min2 || k > max2) ticket | (_, min1, max1, min2, max2) <- conditions]

invalidity :: [(String, Int, Int, Int, Int)] -> [Int] -> Int
invalidity conditions = sum . invalid conditions

naloga1 conditions _ tickets = sum $ map (invalidity conditions) tickets

candidates :: [(String, Int, Int, Int, Int)] -> [Int] -> [[(String, Int)]]
candidates conditions ticket =
  map (map fst) $
    [ filter (\(_, k) -> not (k < min1 || k > max1 && k < min2 || k > max2)) (zip (zip (repeat name) [0 ..]) ticket)
      | (name, min1, max1, min2, max2) <- conditions
    ]

getPossiblePositions :: [(String, Int, Int, Int, Int)] -> [[Int]] -> [[(String, Int)]]
getPossiblePositions conditions tickets = map intersection $ transpose $ map (candidates conditions) tickets

buildMap :: [[(String, Int)]] -> HashMap String Int
buildMap =
  let aux map _ [] = map
      aux map assigned (x : xs) = aux (Map.insert name number map) (Set.insert number assigned) xs
        where
          (name, number) = head $ filter (\t -> snd t `Set.notMember` assigned) x
   in aux Map.empty Set.empty

naloga2 conditions ticket tickets =
  product $ map (\t -> ticket !! (positions ! t)) departureNames
  where
    positions = buildMap $ sortOn length $ getPossiblePositions conditions tickets
    departureNames =
      [ "departure location",
        "departure station",
        "departure platform",
        "departure track",
        "departure date",
        "departure time"
      ]
