import           Data.Bits          (Bits ((.&.), (.|.)))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map (elems, empty, fromList, insert,
                                            union)
import           Data.List          (elemIndex, isPrefixOf)
import qualified Data.List          as List

main = do
  input <- lines <$> readFile "day_14.in"
  writeFile "day_14_1.out" $ show $ naloga1 input
  writeFile "day_14_2.out" $ show $ naloga2 input

replace what with = map (\c -> if c == what then with else c)

toBin :: Int -> String
toBin n =
  let aux 0 = ['0']
      aux n | odd  n = '1' : aux (n `div` 2)
            | even n = '0' : aux (n `div` 2)
   in reverse $ take 36 (aux n ++ repeat '0')

toNum :: String -> Int
toNum =
  let aux []         = 0
      aux ('0' : xs) = 0 + 2 * aux xs
      aux ('1' : xs) = 1 + 2 * aux xs
   in aux . reverse

applyMask :: String -> String -> IntMap Int -> IntMap Int
applyMask cmd mask = Map.insert n (v .&. toNum mask1 .|. toNum mask0)
  where
    n = read $ takeWhile (/= ']') $ drop 4 cmd
    v = read $ drop 4 $ dropWhile (/= ']') cmd
    mask0 = replace 'X' '0' mask
    mask1 = replace 'X' '1' mask

naloga1 =
  let aux mem _ [] = sum $ Map.elems mem
      aux mem mask (cmd : cmds)
        | "mask" `isPrefixOf` cmd = aux mem (drop 7 cmd) cmds
        | "mem[" `isPrefixOf` cmd = aux (applyMask cmd mask mem) mask cmds
   in aux Map.empty ""

applyMask'' :: String -> String -> String -> String
applyMask'' mask base n = [if (b == 'X') || (m == '1') then m else d | (m, b, d) <- zip3 mask base n]

genAllAddresses :: String -> Int -> [Int]
genAllAddresses mask n =
  let aux masks [] = masks
      aux masks (x : xs) =
        if 'X' `elem` x
          then aux (aux masks [take k x ++ ('0' : drop (k+1) x)]) ((take k x ++ ('1' : drop (k+1) x)) : xs)
          else aux (x : masks) xs
        where Just k = elemIndex 'X' x
   in map (\m -> toNum $ applyMask'' m mask $ toBin n) $ aux [] [mask]

applyMask' :: String -> String -> IntMap Int -> IntMap Int
applyMask' cmd mask = Map.union (Map.fromList $ zip (genAllAddresses mask n) (repeat v))
  where
    n = read $ takeWhile (/= ']') $ drop 4 cmd
    v = read $ drop 4 $ dropWhile (/= ']') cmd

naloga2 =
  let aux mem _ [] = sum (Map.elems mem)
      aux mem mask (cmd : cmds)
        | "mask" `isPrefixOf` cmd = aux mem (drop 7 cmd) cmds
        | "mem[" `isPrefixOf` cmd = aux (applyMask' cmd mask mem) mask cmds
   in aux Map.empty ""
