import Data.List.Split
import Data.List
import Data.Char

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

check :: String -> Bool
check s = first || second
    where 
        first = password !! (i1-1) == char && password !! (i2-1) /= char  
        second = password !! (i1-1) /= char && password !! (i2-1) == char  
        password = split !! 2 
        char = (split !! 1) !! 0
        i1 = read $ range !! 0 :: Int
        i2 = read $ range !! 1 :: Int
        range = splitOn "-" $ split !! 0
        split = splitOn " " s 

solve :: [String] -> Int
solve = length . filter check

main :: IO () 
main = interact $ show . solve . lines
