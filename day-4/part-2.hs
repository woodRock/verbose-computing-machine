import Data.List 
import Data.List.Split
import Text.Read
import Text.Regex.Posix

type KeyValue = (String,String)

checkPair :: KeyValue -> Bool
checkPair ("byr",v) = v =~ "^(19[2-9][0-9]|200[0-2])$" 
checkPair ("iyr",v) = v =~ "^(201[0-9]|2020)$"
checkPair ("eyr",v) = v =~ "^(202[0-9]|2030)$"
checkPair ("hgt",v) = v =~ "^(59in|6[0-9]in|7[0-6]in|1[5-8][0-9]cm|19[0-3]cm)$"
checkPair ("hcl",v) = v =~ "^#[0-9a-f]{6}$"
checkPair ("ecl",v) = v =~ "^(amb|blu|brn|gry|grn|hzl|oth)$" 
checkPair ("pid",v) = v =~ "^[0-9]{9}$"
checkPair ("cid",_) = True
checkPair (_,v) = False

check :: String -> Bool
check s = exists && valid && length pairs >= 7
    where
        valid = all (== True) $ map checkPair pairs 
        pairs = map (\x -> (x !! 0, x !! 1)) $ map (splitOn ":") $ splitOn " " formatted
        formatted = map (\c -> if c=='\n' then ' ' else c) s
        exists = s =~ "byr" && s =~"iyr" && s =~ "eyr" && 
                s =~ "hgt" && s =~ "hcl" && s =~ "ecl" && s =~ "pid"

solve :: [String] -> Int 
solve = length . filter check

main :: IO ()
main = interact $ show . solve . splitOn "\n\n" 
