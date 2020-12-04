{-# LANGUAGE BlockArguments #-}

import Data.List.Split
import qualified Data.Map.Strict as Map

main = do
  input <- readFile "day_04.in"
  -- print $ naloga2 input
  -- naloga2 input
  writeFile "day_04_1.out" $ show $ naloga1 input
  writeFile "day_04_2.out" $ show $ naloga2 input

--   writeFile "day_04_1.out" $ show $ map (concatMap (splitOn " ") . splitOn "\n") (splitOn "\n\n" input)

buildPassport :: String -> Map.Map String String
buildPassport s =
  let aux acc [] = acc
      aux acc (x : xs) =
        aux (Map.insert k v acc) xs
        where
          [k, v] = splitOn ":" x
   in aux Map.empty $ concatMap (splitOn " ") $ splitOn "\n" s

verifyPassport :: Map.Map String String -> Bool
verifyPassport = and . mapM Map.member ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

naloga1 input = length . filter verifyPassport $ map buildPassport $ splitOn "\n\n" input

isValidHeight hgt =
  let hgtUnit "" = ""
      hgtUnit (s : ss)
        | s == 'c' = "cm"
        | s == 'i' = "in"
        | otherwise = hgtUnit ss
   in if hgtUnit hgt == "cm"
        then "150cm" <= hgt && hgt <= "193cm"
        else (hgtUnit hgt == "in") && ("59in" <= hgt && hgt <= "76in")

isValidHex ('#' : hex) = length hex == 6 && all (\c -> '0' <= c && c <= '9' || 'a' <= c && c <= 'f') hex
isValidHex _ = False

isValidEyeColour ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPid pid = length pid == 9 && all (\c -> '0' <= c && c <= '9') pid

verifyPassportData :: Map.Map String String -> Bool
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

naloga2 input = length . filter verifyPassportData . filter verifyPassport $ map buildPassport $ splitOn "\n\n" input
