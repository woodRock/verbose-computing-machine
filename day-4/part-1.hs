import Data.List 
import Data.List.Split

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

check :: String -> Bool
check s = all (== True) $ map (\x -> x `isInfixOf` s) required
    where required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

solve :: [String] -> Int 
solve = length . filter check

main :: IO ()
main = interact $ show . solve . splitOn "\n\n" 
