import Data.List (transpose)
import Data.Char (digitToInt)

binarytoint :: [Int] -> Int 
binarytoint a = sum $ map (\(x,y) -> y*2^x) $ zip (reverse [0..n]) a
  where 
    n = (length a) - 1
oxygen :: [[Int]] -> Int -> [Int] 
oxygen a i
  | length f == 1 = f !! 0
  | otherwise = oxygen f (i + 1) 
  where
    f = filter ((== msb).(!! i)) a
    msb = fromEnum $ 2 * sum x >= length x
    x = (!! i) $ transpose a

life :: [[Int]] -> Int -> [Int] 
life a i
  | length f == 1 = f !! 0
  | otherwise = life f (i + 1) 
  where
    f = filter ((== lsb).(!! i)) a
    lsb = fromEnum $ 2 * sum x < length x
    x = (!! i) $ transpose a

solve :: [[Int]] -> Int
solve x = product $ map binarytoint $ [oxygen x 0, life x 0] 

main :: IO () 
main = interact $ show . solve . map (map digitToInt) . lines  
