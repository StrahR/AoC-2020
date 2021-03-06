import           Control.Applicative          (Alternative ((<|>)))
import           Data.Char                    (digitToInt, isAlpha, isDigit,
                                               isSpace)
import qualified Data.Map.Strict              as PlainMap (fromList)
import           Data.MultiMap                (MultiMap)
import qualified Data.MultiMap                as Map (fromList, fromMap, lookup,
                                                      toList)
import qualified Data.Set                     as Set (empty, insert)
import           Text.ParserCombinators.ReadP (eof, many1, option, readP_to_S,
                                               satisfy, string)

main = do
  input <- readFile "day_07.in"
  let input' = lines input :: [String]
  -- print $ map parseRule input'
  -- print $ readP_to_S name "light red bags contain 1 bright white bag, 2 muted yellow bags."
  writeFile "day_07_1.out" $ show $ naloga1 input'
  writeFile "day_07_2.out" $ show $ naloga2 input'

word = many1 (satisfy isAlpha)

space = satisfy isSpace

digit = satisfy isDigit

name = do
  w1 <- word
  space
  w2 <- word
  space
  return (w1 ++ " " ++ w2)

bagType = do
  n <- digit
  space
  colour <- name
  string "bag" <|> string "bags"
  string ", " <|> string "."
  return (digitToInt n, colour)

rule = do
  ruleName <- name
  string "bags contain no other bags." <|> string "bags contain "
  contains <- option [] (many1 bagType)
  eof
  return (ruleName, contains)

parseRule = fst . head . readP_to_S rule

buildRulesBackwards :: [(String, [(Int, String)])] -> MultiMap String String
buildRulesBackwards = Map.fromList . map (\(k, v) -> (snd v, k)) . Map.toList . buildRulesForwards

countParents :: MultiMap String String -> Int
countParents m =
  let aux visited []       = visited
      aux visited (x : xs) = aux (Set.insert x visited) (xs ++ Map.lookup x m)
   in length $ aux Set.empty $ Map.lookup "shiny gold" m

naloga1 = countParents . buildRulesBackwards . map parseRule

buildRulesForwards :: [(String, [a])] -> MultiMap String a
buildRulesForwards = Map.fromMap . PlainMap.fromList

countChildren :: MultiMap String (Int, String) -> Int
countChildren m =
  let aux k [] = k
      aux k ((n, x) : xs) = aux (k + n) (xs ++ map (\(l, m) -> (n*l, m)) (Map.lookup x m))
   in aux 0 $ Map.lookup "shiny gold" m

naloga2 = countChildren . buildRulesForwards . map parseRule
