type Course = [String]
type Level = String
type Trees = [Int]

check :: Trees -> Level -> Int
check x y 
    | y !! position == '#' = 1
    | otherwise = 0
    where
        position = slope `mod` (length y)
        slope = 3 * (length x - 1)
        
solve :: Course -> Int 
solve s = length $ filter (==1) $ foldl (\x y -> check x y : x) [0] s

main :: IO () 
main = interact $ show . solve . lines
