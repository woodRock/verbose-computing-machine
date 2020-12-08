import Data.List

type Pair = (Int, Int)

pairs :: [Int] -> [Pair]
pairs l = nub [(x,y) | x <-l, y<-l, x < y]

check :: Pair -> Bool
check (x,y) = x + y == 2020

solve :: [Int] -> Int
solve a = x * y
    where
        (x,y) = head matches
        matches = filter check $ pairs a

main :: IO () 
main = interact $ show . solve . map read . lines 
