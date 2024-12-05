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

violatesAnyRule :: [Rule] -> Page -> Bool
violatesAnyRule rules page = any (`violatesRule` page) rules

getMiddleNumbers :: [Rule] -> [Page] -> [Int]
getMiddleNumbers rules = map middle . filter (not . violatesAnyRule rules)
    where middle page = page !! (length page `div` 2)

main :: IO ()
main = do
    (rules, pages) <- parseInput <$> getContents
    let middles = getMiddleNumbers rules pages
    mapM_ print middles
    print $ sum middles