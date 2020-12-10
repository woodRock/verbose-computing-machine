import Data.List

tribonacci :: Int -> Int
tribonacci n 
    | n == 0 = 1 
    | n < 0 = 0
    | otherwise = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

solve :: [Int] -> Int 
solve n = product $ map (tribonacci . length) $ filter ((== 1) . head) $ group diff
    where 
        diff = zipWith (-) (tail list) list
        list = 0 : n ++ [maximum n + 3] 

main :: IO () 
main = interact $ show . solve . sort . map read . lines 
