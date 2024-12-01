import Data.List

-- A pair of integers it two integers separated by a space.
readPair :: String -> (Int, Int)
readPair x = (read a, read b)
    where 
        (a:b:_) = words x

-- Distance between to integers.
distance :: Int -> Int -> Int
distance x y = abs (y - x)

-- Count the number of times a number from the lhs appears in the rhs.
count :: Int -> [Int] -> Int
count x y = length $ filter (==x) y

-- Similarity score is the number of occurences on rhs multiplied by the number on the lhs.
similarityScore :: ([Int], [Int]) -> [Int]
similarityScore (x,y) = zipWith (*) x (map (\i -> count i y) x)

main :: IO () 
main = interact $ show . sum . similarityScore . unzip . map readPair . lines