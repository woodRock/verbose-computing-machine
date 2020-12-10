import Data.List

type Triplet = [Int]

triplets :: [Int] -> [Triplet]
triplets l = [[l!!a, l!!b, l!!c] | c<-[0 .. length l - 1],b<-[0..c],a<-[0..b], l!!a + l!!b + l!!c == 2020]

solve :: [Int] -> Int
solve a  
    | null matches = 0
    | otherwise = product $ head matches
    where
        matches = triplets a

main :: IO () 
main = interact $ show . solve . map read . lines 
