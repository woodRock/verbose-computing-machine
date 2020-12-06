-- Right 1, down 1.
-- Right 3, down 1. (This is the slope you already checked.)
-- Right 5, down 1.
-- Right 7, down 1.
-- Right 1, down 2.

type Slope = (Int,Int)

check :: [Int] -> String -> Slope -> Int
check x y (r,d) 
    | y !! position == '#' = 1
    | otherwise = 0
    where
        position = (slope `mod` (length y))
        slope = r * (length x - 1)
        
solveOne :: [String] -> Slope -> Int 
solveOne course (r,d) = length $ filter (==1) $ foldl (\x y -> check x y (r,d): x) [0] row_hits
    where row_hits = [x | (x,i) <- zip course [0..], i `mod` d == 0]

solve :: [String] -> Int
solve course = product $ map (\slope -> solveOne course slope) slopes 
    where slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]

main :: IO () 
main = interact $ show . solve . lines
