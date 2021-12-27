import Data.List (transpose)
import Data.Char (digitToInt)

binarytoint :: [Int] -> Int 
binarytoint a = sum $ map (\(x,y) -> y*2^x) $ zip (reverse [0..n]) a
  where 
    n = (length a) - 1

solve :: [[Int]] -> Int 
solve x = product $ map binarytoint $ [epsilon, gamma] 
  where
    epsilon = map (fromEnum . (== 0)) gamma   
    gamma = map foo $ transpose x 
    foo x = fromEnum $ sum x > ((length x) `div` 2)

main :: IO () 
main = interact $ show . solve . map (map digitToInt) . lines  
