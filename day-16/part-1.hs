import Data.List.Split (splitOn)

type Ticket = [Int]
type Range = (Int,Int)

parseRules :: String -> [Range]
parseRules = concatMap ((\[a,b,c,d] -> [(read a,read b),(read c,read d)]) . 
            concatMap (splitOn "-") . filter (/= "or") . words . last . splitOn ":") . lines

parseTickets :: String -> [Ticket] 
parseTickets = map (map read . splitOn ",") . tail . lines

check :: Ticket -> [Range] -> [Int]
check t r = filter (\x -> all (== False) $ map (\(a,b) -> a <= x && x <= b ) r) t

solve :: [String] -> Int
solve s = sum $ concatMap (`check` r) n 
    where
        m = head $ parseTickets mine
        n = parseTickets nearby
        r = parseRules rules    
        [rules,mine,nearby] = s

main :: IO ()
main = interact $ show . solve . splitOn "\n\n"
