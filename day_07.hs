import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map.Strict as PlainMap (Map, empty, insert, lookup)
import qualified Data.MultiMap as Map (MultiMap, empty, fromMap, insert, lookup, toMap)
import qualified Data.Set as Set (Set, empty, insert, member)
import Text.ParserCombinators.ReadP

main = do
  input <- readFile "day_07.in"
  let input' = lines input :: [String]
  writeFile "day_07_1.out" $ show $ naloga1 input'
  writeFile "day_07_2.out" $ show $ naloga2 input'

word = many1 (satisfy isAlpha)

space = satisfy isSpace

digit = satisfy isDigit

name = do
  w1 <- word
  space
  w2 <- word
  return (w1 ++ " " ++ w2)

bagType = do
  n <- digit
  space
  colour <- name
  string " bag" <|> string " bags"
  string ", " <|> string "."
  return (digitToInt n, colour)

rule = do
  ruleName <- name
  string " bags contain "
  contains <- many1 bagType
  _ <- eof
  return (ruleName, contains)

parseName = readP_to_S name

parseBagType = readP_to_S bagType

parseRule = readP_to_S rule

buildRules = foldl (\map (parent, children) -> foldl (\m c -> Map.insert (snd c) parent m) map children) Map.empty

floodFill m =
  let aux visited [] = visited
      aux visited (x : xs) =
        if Set.member x visited
          then aux visited xs
          else aux (Set.insert x visited) (xs ++ Map.lookup x m)
   in aux Set.empty $ Map.lookup "shiny gold" m

naloga1 = length . floodFill . buildRules . map (fst . head) . filter (not . null) . map parseRule

buildRules' = Map.fromMap . foldl (\m (parent, children) -> PlainMap.insert parent children m) PlainMap.empty

floodFill' m =
  let aux k [] = k
      aux k ((n, x) : xs) =
        aux (k + n) (xs ++ concat (replicate n (Map.lookup x m)))
   in aux 0 $ Map.lookup "shiny gold" m

naloga2 = floodFill' . buildRules' . map (fst . head) . filter (not . null) . map parseRule
