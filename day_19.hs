{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative (Alternative ((<|>)))
import Data.Char
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as Map (fromList)
import Data.List.Split
import Text.ParserCombinators.ReadP
  ( ReadP,
    eof,
    many,
    many1,
    readP_to_S,
    satisfy,
    string,
  )

data Parser
  = Literal Char
  | Exactly Int
  | Seq (Int, Int)
  | Seq3 (Int, Int, Int)
  | Alternative (Parser, Parser)
  deriving (Eq, Show, Read)

main = do
  input <- splitOn "\n\n" <$> readFile "day_19.in"
  let parsers = lines $ head input
  let input' = lines $ input !! 1
  writeFile "day_19_1.out" $ show $ naloga1 parsers input'
  writeFile "day_19_2.out" $ show $ naloga2 parsers input'

number :: ReadP Int
number = read <$> many1 (satisfy isDigit)

spaces :: ReadP String
spaces = many (satisfy isSpace)

ruleLiteral :: ReadP Parser
ruleLiteral = do
  string "\""
  c <- satisfy isAlpha
  string "\""
  return $ Literal c

ruleSeq :: ReadP Parser
ruleSeq = do
  n1 <- number
  satisfy isSpace
  n2 <- number
  return $ Seq (n1, n2)

ruleSeq3 :: ReadP Parser
ruleSeq3 = do
  Seq (n1, n2) <- ruleSeq
  satisfy isSpace
  n3 <- number
  return $ Seq3 (n1, n2, n3)

ruleOpt :: ReadP Parser
ruleOpt = do
  p1 <- Exactly <$> number <|> ruleSeq <|> ruleSeq3
  string " | "
  p2 <- Exactly <$> number <|> ruleSeq <|> ruleSeq3
  return $ Alternative (p1, p2)

rule :: ReadP (Int, Parser)
rule = do
  n <- number
  string ": "
  p <- ruleLiteral <|> Exactly <$> number <|> ruleOpt <|> ruleSeq <|> ruleSeq3
  eof
  return (n, p)

tryParse :: IntMap Parser -> String -> [String]
tryParse parsers ss =
  let parse [] _ = []
      parse acc parser =
        case parser of
          Literal c -> map tail $ filter (\s -> not (null s) && head s == c) acc
          Exactly n -> aux acc n
          Seq (n1, n2) -> aux (aux acc n1) n2
          Seq3 (n1, n2, n3) -> aux (aux (aux acc n1) n2) n3
          Alternative (p1, p2) -> parse acc p1 ++ parse acc p2
      aux acc start = parse acc (parsers ! start)
   in aux [ss] 0

naloga1 parserRules input =
  let parsers = Map.fromList $ map (fst . head . readP_to_S rule) parserRules
   in length $ filter (\(_, out) -> "" `elem` out) $ zip input (map (tryParse parsers) input)

naloga2 parserRules = naloga1 (parserRules ++ ["8: 42 | 42 8", "11: 42 31 | 42 11 31"])
