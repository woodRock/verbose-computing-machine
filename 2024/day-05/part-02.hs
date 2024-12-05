import Data.List (sortBy)

type Rule = (Int, Int)
type Page = [Int]

parseRule :: String -> Rule
parseRule s = let (a,_:b) = span (/='|') s in (read a, read b)

parsePage :: String -> [Int]
parsePage = map read . words . map (\c -> if c==',' then ' ' else c)

parseInput :: String -> ([Rule], [Page])
parseInput s = case span (/="") $ lines s of
    (rules, _:pages) -> (map parseRule rules, map parsePage pages)

violatesRule :: Rule -> Page -> Bool
violatesRule (a,b) page = a `elem` page && b `elem` page && ia > ib
    where
        ia = head [i | (i,n) <- zip [0..] page, n == a]
        ib = head [i | (i,n) <- zip [0..] page, n == b]

isIncorrectlyOrdered :: [Rule] -> Page -> Bool
isIncorrectlyOrdered rules page = any (`violatesRule` page) rules

createOrdering :: [Rule] -> [Int] -> [Int]
createOrdering rules = sortBy (\x y -> 
    if any (\(a,b) -> a==x && b==y) rules then LT
    else if any (\(a,b) -> a==y && b==x) rules then GT
    else compare x y)

main :: IO ()
main = do
    (rules, pages) <- parseInput <$> getContents
    let incorrectPages = filter (isIncorrectlyOrdered rules) pages
        ordered = map (createOrdering rules) incorrectPages
        middles = map (\p -> p !! (length p `div` 2)) ordered
    mapM_ print middles
    print $ sum middles