type BinaryCode = String
type Directions = (Char, Char)
type Indexes = [Int]

bs :: BinaryCode -> Indexes -> Directions -> Int
bs [] i _ = head i
bs (s:xs) i (l,r) 
    | s == l = bs xs left (l,r) 
    | otherwise = bs xs right (l,r)
    where 
        left = take mid i
        right = drop mid i
        mid = length i `div` 2

seatId :: BinaryCode -> Int
seatId b = row * 8 + col
    where 
        row = bs (take 7 b) [0..127] ('F','B')
        col = bs (drop 7 b) [0..7] ('L','R')

solve :: [BinaryCode] -> Int 
solve = maximum . map seatId

main :: IO () 
main = interact $ show . solve . lines
