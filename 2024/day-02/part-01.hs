-- Check if list represents a valid sequence with differences ≤ 3
solveOne :: [Int] -> Int
solveOne xs = fromEnum $ diffOk && (increasing || decreasing)
  where
    pairs = zip xs (tail xs)
    diffOk = all (\(x,y) -> abs (y - x) <= 3) pairs
    increasing = all (\(x,y) -> x < y) pairs
    decreasing = all (\(x,y) -> x > y) pairs

solve :: [[Int]] -> Int
solve = sum . map solveOne

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines