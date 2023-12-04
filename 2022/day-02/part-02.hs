import Data.List.Split (splitOn)

type Round = [[String]]
type MinimumDice = (Int, Int, Int)

add' :: MinimumDice -> MinimumDice -> MinimumDice
add' (a,b,c) (a',b',c') = (a+a',b+b',c+c')

powerset:: MinimumDice -> Int 
powerset (a,b,c) = a * b * c 

match:: Int -> String -> MinimumDice
match score "red" = (score,0,0)
match score "green" = (0,score,0)
match score "blue" = (0,0,score)

match_all:: Round -> MinimumDice
match_all round = foldr add' (0,0,0) 
                (map (\[score, color] -> 
                    match (read score::Int) 
                color) round)

mininum_dice:: MinimumDice -> MinimumDice -> MinimumDice
mininum_dice (a,b,c) (a',b',c') = (max a a', max b b', max c c')

solve :: String -> Int
solve x = power_set
    where 
        [game , results] = splitOn ": " x
        [_, game_no] = splitOn " " game
        game_id = read game_no :: Int
        rounds = splitOn "; " results
        map_rounds = map (\x-> splitOn ", " x) rounds
        map_colors = map (\y -> map (\x-> splitOn " " x) y) map_rounds
        map_dice = map match_all map_colors
        minimum_die = foldr mininum_dice (0,0,0) map_dice
        power_set = powerset minimum_die

main :: IO () 
main = interact $ show  . sum . map solve . lines