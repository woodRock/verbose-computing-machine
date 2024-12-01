import Data.List

-- A pair of integers it two integers separated by a space.
readPair :: String -> (Int, Int)
readPair x = (read a, read b)
    where 
        (a:b:_) = words x

-- Sort the two lists
sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (x,y) = ((sort x), (sort y))

-- Distance between to integers.
distance :: Int -> Int -> Int
distance x y = abs (y - x)

-- Take the difference of the two lists.
diffLists :: ([Int], [Int]) -> [Int]
diffLists (x,y) = zipWith (distance) x y

main :: IO () 
main = interact $ show . sum . diffLists . sortLists . unzip . map readPair . lines