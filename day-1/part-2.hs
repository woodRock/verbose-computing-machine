import Data.List

type Triplet = [Int]

triplets :: [Int] -> [Triplet]
triplets l = [[l !! a, l !! b, l !! c] | c <- [0 .. length l - 1],b <- [0..c],a <- [0..b]]

check :: Triplet -> Bool
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
