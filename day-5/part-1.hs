type BinaryCode = String
type Directions = (Char, Char)
type Indexes = [Int]

bs :: BinaryCode -> Indexes -> Directions -> Int
bs [] i _ = i !! 0
bs (s:xs) i (l,r) 
    | s == l = bs xs left (l,r) 
    | otherwise = bs xs right (l,r)
    where 
        left = take mid i
        right = drop mid i
        mid = length i `div` 2

seat_id :: BinaryCode -> Int
seat_id b = row * 8 + col
    where 
        row = bs (take 7 b) [0..127] ('F','B')
        col = bs (drop 7 b) [0..7] ('L','R')

solve :: [BinaryCode] -> Int 
solve = maximum . map seat_id

main :: IO () 
main = interact $ show . solve . lines
