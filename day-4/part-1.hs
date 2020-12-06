import Data.List 
import Data.List.Split

check :: String -> Bool
check s = all (== True) $ map (\x -> x `isInfixOf` s) required
    where required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

solve :: [String] -> Int 
solve = length . filter check

main :: IO ()
main = interact $ show . solve . splitOn "\n\n" 
