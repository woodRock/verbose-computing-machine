foo :: (Int,Int) -> Int 
foo (a,b) 
  | a < b = 1 
  | otherwise = 0

solve :: [Int] -> Int 
solve x = sum $ map foo $ zip t $ tail t 
  where 
    t = map (\(a,b,c) -> a + b + c) $ zip3 x (tail x) (tail $ tail x) 

main :: IO () 
main = interact $ show . solve . map read . lines
