import Data.List.Split (splitOn)
import Control.Arrow
import Data.List

type Ticket = [Int]
type Column = [Int]
type Rule = (String,[Int])
type Possible = [[String]]
type Definite = [String]

parseRules :: String -> [Rule]
parseRules rs = map (parse . splitOn ":") $ lines rs
    where 
        parse = head &&& range
        range = concatMap ((\[a,b] -> [read a .. read b]) . splitOn "-") . filter (/= "or") . words . last

parseTicket :: String -> [Ticket] 
parseTicket = map (map read . splitOn ",") . tail . lines

discard :: [Ticket] -> [Rule] -> [Ticket]
discard ts rs = filter check ts 
    where   
        check t = all (== True) $ map (\n -> True `elem` map (\r -> n `elem` snd r) rs) t

possible :: [Ticket] -> [Rule] -> Possible
possible ts rs = p
    where
        p = map check (transpose ts) 
        check c = map (fst.head) $ filter (all ((== True).snd)) $ transpose $ map (\n -> map (\r -> (fst r, n `elem` snd r)) rs) c

definite :: Possible -> Definite
definite p
    | solved = map head p
    | otherwise = definite (map (\x -> if length x == 1 then x else filter (`notElem` done) x) p) 
    where
        done = concat $ filter ((== 1). length) p
        solved = length p == sum (map length p)  

solve :: [String] -> Int
solve [r,m,n] = product $ map ((mine !!). fst) $ filter (("departure" `isInfixOf`). snd) $ zip [0..] (definite (possible valid rules))
    where
        rules = parseRules r
        mine = head $ parseTicket m
        valid = discard nearby rules
        nearby = parseTicket n

main :: IO ()
main = interact $ show . solve . splitOn "\n\n"
