-- Solve a geometric series to give the next number in the sequence
-- e.g. 0,3,6,9,12,15 -> 18 

diff :: [Int] -> [Int]
diff [x,y] = [y - x]
diff (x:y:xs) = (y - x) : diff (y:xs)

expand :: [Int] -> [[Int]] 
expand x 
    | all ((==) 0) x = [x]
    | otherwise = x : expand (diff x) 

solve:: [[Int]] -> Int
solve x = previous + delta
    where 
        (previous:rest) = map last x
        delta = sum rest

parse :: String -> [Int]
parse = map read . words

main:: IO()
main = interact $ show . map (solve . expand . parse) . lines 