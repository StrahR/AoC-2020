import Data.Char
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map (fromList, insert, size)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Text.ParserCombinators.ReadP (eof, many1, readP_to_S, satisfy)

main = do
  input <- readFile "day_08.in"
  let input' = loadProgramToMemory $ map (fst . head . parseOp) $ lines input :: Map Int (String, Int)
  let input'' = Map.insert (Map.size input') ("end", 0) input'
  --   print $ parseOp (input' ! 0)
  writeFile "day_08_1.out" $ show $ naloga1 input''
  writeFile "day_08_2.out" $ show $ naloga2 input''

-- loadProgramToMemory =
--     let aux mem _ []     = mem
--         aux mem n (x:xs) = aux (Map.insert n x mem) (n+1) xs
--      in aux Map.empty 0
word = many1 (satisfy isAlpha)

space = satisfy isSpace

number = many1 (satisfy isDigit)

opP = do
  optcode <- word
  space
  pm <- satisfy isAscii
  arg <- number
  let arg' = if pm == '-' then - read arg :: Int else read arg
  _ <- eof
  return (optcode, arg')

parseOp = readP_to_S opP

loadProgramToMemory :: [a] -> Map Int a
loadProgramToMemory xs = Map.fromList $ zip [0 .. length xs] xs

handheldSim :: Map Int (String, Int) -> (Int, Int)
handheldSim mem =
  let call pc acc hist
        | not (Set.member pc hist) =
          aux optcode arg pc acc (Set.insert pc hist)
        where
          (optcode, arg) = mem ! pc
      call _ acc _ = (1, acc)
      aux "acc" arg pc acc hist = call (pc + 1) (acc + arg) hist
      aux "jmp" arg pc acc hist = call (pc + arg) acc hist
      aux "nop" _ pc acc hist = call (pc + 1) acc hist
      aux "end" _ _ acc _ = (0, acc)
   in call 0 0 Set.empty

naloga1 = snd . handheldSim

handheldSimSub :: [(Int, (String, Int))] -> Map Int (String, Int) -> (Int, Int)
handheldSimSub [] = handheldSim
handheldSimSub ((pc, op) : xs) = handheldSimSub xs . Map.insert pc op

naloga2 mem =
  let aux pc =
        case optcode of
          "acc" -> aux (pc + 1)
          "jmp" ->
            let (r, v) = handheldSimSub [(pc, ("nop", arg))] mem
             in if r == 0 then v else aux (pc + 1)
          "nop" ->
            let (r, v) = handheldSimSub [(pc, ("jmp", arg))] mem
             in if r == 0 then v else aux (pc + 1)
        where
          (optcode, arg) = mem ! pc
   in aux 0
