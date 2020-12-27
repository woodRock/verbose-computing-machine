import Data.List.Split (splitOn)
import Control.Arrow

type Hand = [Int]
type Score = Int

parse :: [String] -> (Hand,Hand)
parse = (\[x,y] -> (x,y)) . map go
    where 
        go = map read . tail . lines 

add :: ([Hand],[Hand]) -> (Hand,Hand) -> ([Hand],[Hand])
add (h1,h2) (p1,p2) = (h1 ++ [p1], h2 ++ [p2])

turn :: (Hand,Hand) -> ([Hand],[Hand]) ->(Bool,Hand)
turn (x,[]) _ = (True,x)
turn ([],x) _ = (False,x)
turn (p1@(x:xs),p2@(y:ys)) (h1,h2)
    | p1 `elem` h1 && p2 `elem` h2 = turn (p1,[]) ([],[])
    | length xs < x || length ys < y = (`turn` hands) $ if x > y then p1Win else p2Win
    | fst recurse = turn p1Win hands
    | otherwise = turn p2Win hands
    where 
        hands = add (h1,h2) (p1,p2)
        recurse = turn (r1,r2) ([],[])
        r1 = take x xs
        r2 = take y ys
        p1Win = (xs ++ [x,y], ys)
        p2Win = (xs, ys ++ [y,x])

score :: Hand -> Score
score = sum . zipWith (*) [1..] . reverse

game :: (Hand,Hand) -> Score
game = score . snd . (`turn` ([[]],[[]]))

main :: IO () 
main = interact $ show . game . parse . splitOn "\n\n"
