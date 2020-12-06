import Data.List

triplets :: [Int] -> [[Int]]
triplets l = nub $ map sort $ map (take 3) $ permutations l

check :: [Int] -> Bool
check t = sum(t) == 2020

solve :: [Int] -> Int
solve a  
    | length matches == 0 = 0 
    | otherwise = product $ matches !! 0
    where
        matches = filter check t
        t  = triplets a

main :: IO () 
main = interact $ show . solve . map read . lines 
