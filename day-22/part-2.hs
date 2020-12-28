import Data.List.Split (splitOn)
import Control.Arrow

type Hand = [Int]
type P1Won = Bool
type Score = Int

parse :: [String] -> (Hand,Hand)
parse = (\[x,y] -> (x,y)) . map (map read . tail . lines)

turn :: (Hand,Hand) -> ([Hand],[Hand]) -> (P1Won,Hand)
turn (x,[]) _ = (True,x)
turn ([],x) _ = (False,x)
turn (p1@(x:xs),p2@(y:ys)) (h1,h2)
    | p1 `elem` h1 && p2 `elem` h2 = (True,p1)
    | length xs < x || length ys < y = (`turn` hands) $ if x > y then p1Win else p2Win
    | p1Won = turn p1Win hands
    | otherwise = turn p2Win hands
    where 
        (p1Won,_) = turn (take x xs, take y ys) ([],[])
        p1Win = (xs ++ [x,y], ys)
        p2Win = (xs, ys ++ [y,x])
        hands = (h1 ++ [p1], h2 ++ [p2])

score :: Hand -> Score
score = sum . zipWith (*) [1..] . reverse

solve :: [String] -> Score
solve = score . snd . (`turn` initial) . parse
    where initial = ([],[])

main :: IO () 
main = interact $ show . solve . splitOn "\n\n"
