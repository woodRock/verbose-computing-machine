import Data.List.Split
import Data.List
import Data.Char

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

check :: String -> Bool
check s = c >= min && c <= max
    where
        c = count char password
        password = split !! 2 
        char = (split !! 1) !! 0
        min = read $ range !! 0 :: Int
        max = read $ range !! 1 :: Int
        range = splitOn "-" $ split !! 0
        split = splitOn " " s 

solve :: [String] -> Int
solve = length . filter check

main :: IO () 
main = interact $ show . solve . lines
