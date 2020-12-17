import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Text.Read
import Text.Regex.Posix

type Ticket = [Int]
type Column = [Int]
type Rule = [Range]
type Range = (Int,Int)

-- TODO Discard invalid values
parseDepart :: String -> [Rule]
parseDepart = map ((\[a,b,c,d] -> [(read a,read b),(read c,read d)]) . 
            concatMap (splitOn "-") . filter (/= "or") . words . last . splitOn ":") . filter (=~ "departure") . lines

parseRules :: String -> [Rule]
parseRules = map ((\[a,b,c,d] -> [(read a,read b),(read c,read d)]) . 
            concatMap (splitOn "-") . filter (/= "or") . words . last . splitOn ":") . lines

parseTicket :: String -> [Ticket] 
parseTicket = map (map read . splitOn ",") . tail . lines

checkCol :: Column -> [Rule] -> Bool
checkCol col rules = any (all (== True)) (transpose j)
    where
        j = map (\i -> map (\[(a,b),(c,d)] -> (a <= i && i <= b) || (c <= i && i <= d) ) rules) col

discard :: [Ticket] -> [Rule] -> [Ticket]
discard ts r = filter go ts
    where
        go = not . any (== True) . map (\x -> all (== False) $ map (\(a,b) -> a <= x && x <= b ) (concat r))

check :: [Column] -> [Rule] -> [Int]
check cols rules = map fromJust $ filter (/= Nothing) $ map (\(c,i) -> if checkCol c rules then Just i else Nothing) (zip cols [0..]) 

solve :: [String] -> [Int] 
solve s = check n r-- product $ map (\i -> m !! i) $ check n r 
    where
        r = parseDepart rules 
        m = head $ parseTicket mine
        n = transpose $ discard (parseTicket nearby) (parseRules rules)
        [rules,mine,nearby] = s

main :: IO ()
main = interact $ unlines . map show . solve . splitOn "\n\n"
