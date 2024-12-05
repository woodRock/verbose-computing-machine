import Data.List (sortBy)

type Rule = (Int, Int)
type Page = [Int]

parseRule :: String -> Rule
parseRule s = let (a,_:b) = span (/='|') s in (read a, read b)

parsePage :: String -> [Int]
parsePage = map read . words . map (\c -> if c==',' then ' ' else c)

parseInput :: String -> ([Rule], [Page])
parseInput = (\(r,_:p) -> (map parseRule r, map parsePage p)) . span (/="") . lines

violatesRule :: Rule -> Page -> Bool
violatesRule (a,b) p = a `elem` p && b `elem` p && idx a p > idx b p
 where idx n = head . map fst . filter ((==n) . snd) . zip [0..]

createOrdering :: [Rule] -> [Int] -> [Int]
createOrdering rules = sortBy (\x y -> 
   if any ((==) (x,y) . \(a,b) -> (a,b)) rules then LT
   else if any ((==) (y,x) . \(a,b) -> (a,b)) rules then GT
   else compare x y)

main :: IO ()
main = do
   (rules, pages) <- parseInput <$> getContents
   let result = map (middle . createOrdering rules) $ filter (\p -> any (\r -> r `violatesRule` p) rules) pages
       middle p = p !! (length p `div` 2)
   print $ sum result