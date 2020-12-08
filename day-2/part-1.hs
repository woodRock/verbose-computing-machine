import Data.List.Split
import Data.List
import Data.Char

type Password = String

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

check :: Password -> Bool
check s = c >= min && c <= max
    where
        c = count char password
        password = split !! 2 
        char = head (split !! 1)
        min = read $ head range :: Int 
        max = read $ range !! 1 :: Int
        range = splitOn "-" $ head split
        split = words  s 

solve :: [Password] -> Int
solve = length . filter check

main :: IO () 
main = interact $ show . solve . lines
