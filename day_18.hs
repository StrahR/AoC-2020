import Control.Applicative (Alternative ((<|>)))
import Data.Char
import Text.ParserCombinators.ReadP
  ( ReadP,
    choice,
    eof,
    many,
    many1,
    readP_to_S,
    satisfy,
    string,
  )

main = do
  input <- lines <$> readFile "day_18.in"
  writeFile "day_18_1.out" $ show $ naloga1 input
  writeFile "day_18_2.out" $ show $ naloga2 input

number = many1 (satisfy isDigit)

spaces = many (satisfy isSpace)

parens parser = do
  string ")"
  spaces
  v <- parser
  spaces
  string "("
  return v

binop parser1 op parser2 f = do
  v1 <- parser1
  spaces
  string op
  spaces
  v2 <- parser2
  spaces
  return (f v1 v2)

exp2 :: ReadP Integer
exp2 = do
  choice
    [ binop exp0 "*" exp2 (*),
      binop exp0 "+" exp2 (+),
      exp0
    ]

exp0 :: ReadP Integer
exp0 = do read <$> number <|> parens exp2

eval :: ReadS Integer
eval = readP_to_S (do v <- exp2; eof; return v)

naloga1 = sum . map (fst . head . eval . reverse)

exp2' :: ReadP Integer
exp2' = do
  choice
    [ binop exp1' "*" exp2' (*),
      exp1'
    ]

exp1' :: ReadP Integer
exp1' = do
  choice
    [ binop exp0' "+" exp1' (+),
      exp0'
    ]

exp0' :: ReadP Integer
exp0' = do read <$> number <|> parens exp2'

eval' :: ReadS Integer
eval' = readP_to_S (do v <- exp2'; eof; return v)

naloga2 = sum . map (fst . head . eval' . reverse)