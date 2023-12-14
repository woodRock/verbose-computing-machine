import Data.List.Split (splitOn)
import Data.List (tails, sort, group)
import Control.Applicative

-- Here is one line of input.
-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 5
-- We can split on "|", then split on ":", then split on " " to get the numbers.

type CardNo = Int -- ubique identifier for a card.
type Score = Int -- number of matches for given card.
type Scorecards = Int -- total number of score cards.
type Winning = [Int]
type Numbers = [Int]
type Multiplier = Int -- Multiplier for each card.
type Card = (Winning, Numbers)
type Matches = (CardNo, [Int])

parse :: String -> Card 
parse x = (winning, numbers)
    where 
        [winning_str, numbers_str] = splitOn "|" x
        right_of = head $ tail $ splitOn ":" winning_str
        numbers = map read $ words numbers_str :: Numbers
        winning = map read $ words right_of :: Winning 

score :: CardNo -> Score -> Card -> Matches
score cn m ([], numbers) = (cn, [cn + 1 .. cn + m])
score cn m (w:ws, numbers)
    | w `elem` numbers = score cn (m + 1) (ws, numbers)
    | otherwise = score cn m (ws, numbers)

solve:: [Multiplier] -> [Matches] -> Scorecards
solve multipliers matches = sum $ zipWith (*) multipliers $ map (length . snd) matches

magic:: String -> String
magic x = show $ matches
    where 
        matches = map (\(cn, c) -> score cn 0 c) . zip [1.. ] . map parse . lines $ x
        multipliers = group . sort . concat . map snd $ matches

main :: IO () 
main = interact magic