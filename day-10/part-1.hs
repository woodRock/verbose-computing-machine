import Data.List

solve :: [Int] -> Int
solve a = product $ map length $ group $ sort $ zipWith (-) (tail l) l 
    where l = 0 : a ++ [maximum a + 3]

main :: IO () 
main = interact $ show . solve . sort . map read . lines 
