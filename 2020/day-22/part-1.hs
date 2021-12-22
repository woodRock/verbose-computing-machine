import Data.List.Split (splitOn)
import Control.Arrow

type Hand = [Int]
type Score = Int

parse :: [String] -> (Hand,Hand)
parse = (\[x,y] -> (x,y)) . map go
    where 
        go = map read . tail . lines 

turn :: (Hand,Hand) -> Hand
turn ([],x) = x
turn (x,[]) = x
turn (x:xs,y:ys)
    | x > y = turn (xs ++ [x,y], ys)
    | otherwise = turn (xs, ys ++ [y,x])

score :: Hand -> Score
score = sum . zipWith (*) [1..] . reverse

game :: (Hand,Hand) -> Score
game = score . turn

main :: IO () 
main = interact $ show . game . parse . splitOn "\n\n"
