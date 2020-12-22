{-# LANGUAGE ViewPatterns #-}

import Data.List.Split
import Data.List
import Data.Char

type Password = String

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

check :: Password -> Bool
check (words -> split) = first || second
    where
        first = password !! (i1-1) == char && password !! (i2-1) /= char
        second = password !! (i1-1) /= char && password !! (i2-1) == char
        password = split !! 2
        char = head (split !! 1)
        i1 = read $ head range
        i2 = read $ range !! 1 :: Int
        range = splitOn "-" $ head split
        split = words s

solve :: [Password] -> Int
solve = length . filter check

main :: IO ()
main = interact $ show . solve . lines
