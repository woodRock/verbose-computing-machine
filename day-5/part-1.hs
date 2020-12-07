type Directions = (Char,Char)

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
        row = bs row_str [0..127] ('F','B')
        col = bs col_str [0..7] ('L','R')
        row_str = take 7 b 
        col_str = drop 7 b

solve :: [String] -> Int 
solve = maximum . map seat_id

main :: IO () 
main = interact $ show . solve . lines
