import Data.List.Split (splitOn)

match:: Int -> String -> Bool
match score "red"
    | score <= 12 = True
    | otherwise = False
match score "green"
    | score <= 13 = True
    | otherwise = False
match score "blue"
    | score <= 14 = True
    | otherwise = False

solve :: String -> Int
solve x = possible_games
    where 
        [game , results] = splitOn ": " x
        [_, game_no] = splitOn " " game
        game_id = read game_no :: Int
        rounds = splitOn "; " results
        map_rounds = map (\x-> splitOn ", " x) rounds
        map_colors = map (\y -> map (\x-> splitOn " " x) y) map_rounds
        scores = map (\round -> map (\[score, color] -> match (read score::Int) color) round) map_colors
        possible_games = if all (==True) (concat scores) then game_id else 0 

main :: IO () 
main = interact $ show  . sum . map solve . lines