import Data.List

check :: [Int] -> (Int, Int)
check a = (ones, threes)  
    where
        [ones, threes] = map length $ group $ sort $ zipWith (-) (tail l) l 
        l = 0 : a ++ [maximum a + 3]

solve :: [Int] -> Int 
solve  = uncurry (*) . check . sort

main :: IO () 
main = interact $ show . solve . map read . lines 
