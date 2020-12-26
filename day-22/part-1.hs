import Data.List.Split (splitOn)
import Control.Arrow

type Hand = [Int]
type Score = Int

parse :: [String] -> (Hand,Hand)
parse = (\[x,y] -> (x,y)) . map go
    where 
        go = map read . tail . lines 

turn :: (Hand,Hand) -> (Hand,Hand)
turn (p1@(x:xs),p2@(y:ys))
    | x > y = (xs ++ [x,y], ys)
    | otherwise = (xs, ys ++ [y,x])

score :: Hand -> Score
score = sum . zipWith (*) [1..] . reverse

game :: (Hand,Hand) -> Score
game = score . winner . head . take 1 . dropWhile playing . iterate turn
    where 
        playing (p1,p2) = not (null p1) && not (null p2)
        winner ([],p2) = p2
        winner (p1,[]) = p1

main :: IO () 
main = interact $ show . game . parse . splitOn "\n\n"
