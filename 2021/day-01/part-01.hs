foo :: (Int,Int) -> Int 
foo (a,b) 
  | a < b = 1 
  | otherwise = 0

solve :: [Int] -> Int 
solve x = sum $ map foo $ zip x $ tail x 

main :: IO () 
main = interact $ show . solve . map read . lines
