import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map.Strict as PlainMap (Map, empty, fromList)
import qualified Data.MultiMap as Map (MultiMap, empty, fromList, fromMap, lookup, toList)
import qualified Data.Set as Set (Set, empty, insert, member)
import Text.ParserCombinators.ReadP

main = do
  input <- readFile "day_07.in"
  let input' = lines input :: [String]
  -- print $ map parseRule input'
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
  string " bags contain no other bags." <|> string " bags contain "
  contains <- option [] (many1 bagType)
  eof
  return (ruleName, contains)

parseRule = fst . head . readP_to_S rule

-- buildRulesBackwards = foldl (\map (parent, children) -> foldl (\m c -> Map.insert (snd c) parent m) map children) Map.empty
buildRulesBackwards = Map.fromList . map (\(k, v) -> (snd v, k)) . Map.toList . buildRulesForwards

countParents m =
  let aux visited [] = visited
      aux visited (x : xs) = aux (Set.insert x visited) (xs ++ Map.lookup x m)
   in length $ aux Set.empty $ Map.lookup "shiny gold" m

naloga1 = countParents . buildRulesBackwards . map parseRule

-- buildRulesForwards = Map.fromMap . foldl (\m (parent, children) -> PlainMap.insert parent children m) PlainMap.empty
buildRulesForwards = Map.fromMap . PlainMap.fromList

countChildren m =
  let aux k [] = k
      aux k ((n, x) : xs) = aux (k + n) (xs ++ concat (replicate n (Map.lookup x m)))
   in aux 0 $ Map.lookup "shiny gold" m

naloga2 = countChildren . buildRulesForwards . map parseRule
