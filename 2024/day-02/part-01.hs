-- Calculate the distance between two numbers.
distance :: Int -> Int -> Int
distance x y = abs (y - x)

-- Check if they are all increasing or decreasing.
-- Check whether difference is less or equal to 3.
solveOne :: [Int] -> Int
solveOne x = if ((increasing || decreasing) && diff3) then 1 else 0
    where 
        diff3 = all (<=3) (zipWith (distance) x (tail x))
        increasing = all (==True) (zipWith (<) x (tail x))
        decreasing = all (==True) (zipWith (>) x (tail x))

solve :: [[Int]] -> Int
solve = sum . map solveOne

main :: IO () 
main = interact $ show . solve . map (map read) . map words . lines