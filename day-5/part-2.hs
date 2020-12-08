import Data.List

type BinaryCode = String
type Indexes = [Int]
type Direction = (Char, Char)
type SeatId = Int

bs :: BinaryCode -> Indexes -> Direction -> Int
bs [] i _ = head i 
bs (s:xs) i (l,r) 
    | s == l = bs xs left (l,r) 
    | otherwise = bs xs right (l,r)
    where 
        left = take mid i
        right = drop mid i
        mid = length i `div` 2

seatId :: BinaryCode -> SeatId
seatId b = row * 8 + col
    where 
        row = bs (take 7 b) [0..127] ('F','B')
        col = bs (drop 7 b) [0..7] ('L','R')

emptySeat :: [SeatId] -> SeatId
emptySeat [] = 0
emptySeat [x] = 0
emptySeat (x:y:xs)
    | x == 0 = emptySeat xs
    | next /= y = next + emptySeat xs
    | otherwise = emptySeat xs
    where next = x + 1

solve :: [BinaryCode] -> SeatId 
solve = emptySeat . sort . map seatId

main :: IO () 
main = interact $ show . solve . lines
