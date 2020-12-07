import Data.List

type Pair = (Int, Int)

uniq_pairs :: [Int] -> [Pair]
uniq_pairs l = nub [(x,y) | x <-l, y<-l, x < y]

check :: Pair -> Bool
check (x,y) = x + y == 2020

solve :: [Int] -> Int
solve a = x * y
    where
        x = fst $ matches !! 0
        y = snd $ matches !! 0
        matches = filter check pairs
        pairs = uniq_pairs a

main :: IO () 
main = interact $ show . solve . map read . lines 
