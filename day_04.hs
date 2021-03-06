{-# LANGUAGE BlockArguments #-}

import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, insert, lookup, member)

main = do
  input <- readFile "day_04.in"
  let input' = map buildPassport $ splitOn "\n\n" input
  -- print $ naloga2 input
  -- naloga2 input
  writeFile "day_04_1.out" $ show $ naloga1 input'
  writeFile "day_04_2.out" $ show $ naloga2 input'

--   writeFile "day_04_1.out" $ show $ map (concatMap (splitOn " ") . splitOn "\n") (splitOn "\n\n" input)
buildPassport :: String -> Map String String
buildPassport s =
  let aux acc [] = acc
      aux acc (x : xs) = aux (Map.insert k v acc) xs
        where
          [k, v] = splitOn ":" x
   in aux Map.empty $ concatMap (splitOn " ") $ splitOn "\n" s

verifyPassport :: Map String String -> Bool
verifyPassport = and . mapM Map.member ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

naloga1 = length . filter verifyPassport

isValidHeight hgt =
  let hgtUnit ""        = ""
      hgtUnit ('c' : _) = "cm"
      hgtUnit ('i' : _) = "in"
      hgtUnit (_ : ss)  = hgtUnit ss
   in case hgtUnit hgt of
        "cm" -> "150cm" <= hgt && hgt <= "193cm"
        "in" -> "59in" <= hgt && hgt <= "76in"
        _    -> False

isValidHex (h : hex) = h == '#' && length hex == 6 && all (\c -> '0' <= c && c <= '9' || 'a' <= c && c <= 'f') hex

isValidEyeColour ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPid pid = length pid == 9 && all (\c -> '0' <= c && c <= '9') pid

verifyPassportData :: Map String String -> Bool
verifyPassportData passport =
  and
    [ "1920" <= byr && byr <= "2002",
      "2010" <= iyr && iyr <= "2020",
      "2020" <= eyr && eyr <= "2030",
      isValidHeight hgt,
      isValidHex hcl,
      isValidEyeColour ecl,
      isValidPid pid
    ]
  where
    Just byr = Map.lookup "byr" passport
    Just iyr = Map.lookup "iyr" passport
    Just eyr = Map.lookup "eyr" passport
    Just hgt = Map.lookup "hgt" passport
    Just hcl = Map.lookup "hcl" passport
    Just ecl = Map.lookup "ecl" passport
    Just pid = Map.lookup "pid" passport

naloga2 = length . filter verifyPassportData . filter verifyPassport
