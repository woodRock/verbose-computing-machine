import Data.List

bs :: String -> [Int] -> (Char, Char) -> Int
bs [] i _ = i !! 0
bs (s:xs) i (l,r) 
    | s == l = bs xs left (l,r) 
    | otherwise = bs xs right (l,r)
    where 
        left = take mid i
        right = drop mid i
        mid = length i `div` 2

seat_id :: String -> Int
seat_id b = row * 8 + col
    where 
        row = bs (take 7 b) [0..127] ('F','B')
        col = bs (drop 7 b) [0..7] ('L','R')

empty_seat :: [Int] -> Int
empty_seat [] = 0
empty_seat [x] = 0
empty_seat (x:y:xs)
    | x == 0 = empty_seat xs
    | next /= y = next + empty_seat xs
    | otherwise = empty_seat xs
    where next = x + 1

solve :: [String] -> Int 
solve = empty_seat . sort . map seat_id

main :: IO () 
main = interact $ show . solve . lines
