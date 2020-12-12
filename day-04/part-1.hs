import Data.List 
import Data.List.Split

type Passport = String

check :: Passport -> Bool
check s = all (== True) $ map (`isInfixOf` s) required
    where required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

solve :: [Passport] -> Int 
solve = length . filter check

main :: IO ()
main = interact $ show . solve . splitOn "\n\n" 
