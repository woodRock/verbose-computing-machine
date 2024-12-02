-- Calculate the distance between two numbers.
distance :: Int -> Int -> Int
distance x y = abs (y - x)

-- Check if they are all increasing or decreasing.
-- Check whether difference is less or equal to 3.
solveOne :: [Int] -> Bool
solveOne x = ((increasing || decreasing) && diff3)
    where 
        diff3 = all (<=3) (zipWith (distance) x (tail x))
        increasing = all (==True) (zipWith (<) x (tail x))
        decreasing = all (==True) (zipWith (>) x (tail x))

-- All permutations of removing one element from the list.
removeOne :: [Int] -> [[Int]]
removeOne [] = []
removeOne (x:xs) = xs : map (x:) (removeOne xs)

-- Check all permutations to see if one is safe. Return the number of safe permutations.
solve :: [[Int]] -> Int
solve =  length . filter (==True) . map (any (==True)) . map (map solveOne) . map (removeOne)

main :: IO () 
main = interact $ show . solve . map (map read) . map words . lines