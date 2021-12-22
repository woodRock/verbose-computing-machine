foo :: (Int,Int) -> Int 
foo (a,b) = fromEnum $ a < b 

solve :: [Int] -> Int 
solve x = sum $ map foo $ zip x $ tail x 

main :: IO () 
main = interact $ show . solve . map read . lines
