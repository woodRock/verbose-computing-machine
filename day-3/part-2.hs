type Slope = (Int,Int)
type Course = [String]
type Level = String
type Trees = [Int]

check :: Trees -> Level -> Slope -> Int
check x y (r,d) 
    | y !! position == '#' = 1
    | otherwise = 0
    where
        position = slope `mod` length y
        slope = r * (length x - 1)
        
solveOne :: Course -> Slope -> Int 
solveOne course (r,d) = length $ filter (==1) $ foldl (\x y -> check x y (r,d): x) [0] row_hits
    where row_hits = [x | (x,i) <- zip course [0..], i `mod` d == 0]

solve :: Course -> Int
solve course = product $ map (solveOne course) slopes 
    where slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]

main :: IO () 
main = interact $ show . solve . lines
