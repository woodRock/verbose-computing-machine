-- Check if list represents a monotonic sequence with differences ≤ 3.
solveOne :: [Int] -> Bool
solveOne xs = (increasing || decreasing) && diffOk
  where
    pairs = zip xs (tail xs)
    diffOk = all (\(x,y) -> abs (y - x) <= 3) pairs
    increasing = all (\(x,y) -> x < y) pairs
    decreasing = all (\(x,y) -> x > y) pairs

-- Generate all subsequences with one element removed.
removeOne :: [Int] -> [[Int]]
removeOne [] = []
removeOne (x:xs) = xs : map (x:) (removeOne xs)

-- Count sequences that have at least one valid subsequence.
solve :: [[Int]] -> Int
solve = length . filter (any solveOne . removeOne) 

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines