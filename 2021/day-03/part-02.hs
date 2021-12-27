import Data.List (transpose)
import Data.Char (digitToInt)

binarytoint :: [Int] -> Int 
binarytoint a = sum $ map (\(x,y) -> y*2^x) $ zip (reverse [0..n]) a
  where n = (length a) - 1

rating :: [[Int]] -> Int -> String -> [Int] 
rating a i r
  | length f == 1 = f !! 0
  | otherwise = rating f (i + 1) r 
  where
    f = filter ((== fromEnum sb).(!! i)) a
    sb 
      | r == "life" = 2 * sum x < length x  
      | r == "oxygen" = 2 * sum x >= length x
    x = (!! i) $ transpose a

solve :: [[Int]] -> Int
solve x = product $ map binarytoint $ [rating x 0 "life", rating x 0 "oxygen"] 

main :: IO () 
main = interact $ show . solve . map (map digitToInt) . lines  
