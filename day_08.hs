import           Data.Char
import           Data.Map.Strict              (Map, (!))
import qualified Data.Map.Strict              as Map (fromList, insert, size)
import qualified Data.Set                     as Set (empty, insert, member)
import           Text.ParserCombinators.ReadP (eof, many1, readP_to_S, satisfy)

main = do
  input <- readFile "day_08.in"
  let input' = loadProgramToMemory $ map (fst . head . parseOp) $ lines input :: Map Int (String, Int)
  let input'' = Map.insert (Map.size input') ("end", 0) input'
  -- print $ parseOp "jmp +333"
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
  eof
  let arg' = if pm == '-' then - read arg else read arg :: Int
  return (optcode, arg')

parseOp = readP_to_S opP

loadProgramToMemory :: [a] -> Map Int a
loadProgramToMemory xs = Map.fromList $ zip [0 .. length xs] xs

handheldSim :: Map Int (String, Int) -> (Bool, Int)
handheldSim mem =
  let dispatch ("acc", arg) pc acc hist = exec (pc + 1) (acc + arg) hist
      dispatch ("jmp", arg) pc acc hist = exec (pc + arg) acc hist
      dispatch ("nop", _)   pc acc hist = exec (pc + 1) acc hist
      dispatch ("end", _)   _  acc _    = (True, acc)
      exec pc acc hist =
        if not (Set.member pc hist)
          then dispatch (mem ! pc) pc acc (Set.insert pc hist)
          else (False, acc)
   in exec 0 0 Set.empty

naloga1 = snd . handheldSim

handheldSimSub :: [(Int, (String, Int))] -> Map Int (String, Int) -> (Bool, Int)
handheldSimSub []              = handheldSim
handheldSimSub ((pc, op) : xs) = handheldSimSub xs . Map.insert pc op

naloga2 mem =
  let replace pc ("acc", _)   = replace (pc + 1) (mem ! (pc+1))
      replace pc ("jmp", arg) = handle pc $ handheldSimSub [(pc, ("nop", arg))] mem
      replace pc ("nop", arg) = handle pc $ handheldSimSub [(pc, ("jmp", arg))] mem
      handle pc (end, val) = if end then val else replace (pc + 1) (mem ! (pc+1))
   in replace 0 (mem ! 0)
