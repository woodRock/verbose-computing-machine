import Data.List.Split (splitOn)

-- Here is one line of input.
-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 5
-- We can split on "|", then split on ":", then split on " " to get the numbers.

type Score = Int
type Winning = [Int]
type Numbers = [Int]
type Card = (Winning, Numbers)

solve :: Score -> Card -> Score
solve score ([], numbers) = score
solve 0 (w:ws, numbers) -- First match is worth one point.
    | w `elem` numbers = solve 1 (ws, numbers)
    | otherwise = solve 0 (ws, numbers)
solve score (w:ws, numbers) -- Score doubles after first match.
    | w `elem` numbers = solve (score * 2) (ws, numbers)
    | otherwise = solve score (ws, numbers)

parse :: String -> Card 
parse x = (winning, numbers)
    where 
        [winning_str, numbers_str] = splitOn "|" x
        right_of = head $ tail $ splitOn ":" winning_str
        numbers = map read $ words numbers_str :: Numbers
        winning = map read $ words right_of :: Winning

main:: IO() 
main = interact $ show . sum . map (solve 0) . map parse . lines 