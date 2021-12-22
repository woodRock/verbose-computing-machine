{-# LANGUAGE ViewPatterns #-}

import Data.List

type Pair = (Int, Int)

pairs :: [Int] -> [Pair]
pairs l = nub [(x,y) | x <-l, y<-l, x < y, x + y == 2020]

solve :: [Int] -> Int
solve (pairs -> matches) = uncurry (*) $ head matches

main :: IO ()
main = interact $ show . solve . map read . lines
