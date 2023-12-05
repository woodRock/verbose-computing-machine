import Data.Char (isDigit)
import Data.List (tails)

type Value = String 
type YIndex = Int
type XIndex = Int
type Start = XIndex
type End = XIndex
type Position = (Value, YIndex, Start, End)
type Number = Position
type Gear = Position

parse :: String -> YIndex -> XIndex -> [Position] -> [Position]
parse [] y_index x_index positions = positions   
parse ('*':xs) y_index x_index positions -- Gear
    = parse xs y_index (x_index + 1) (positions ++ [('*':[], y_index, x_index, x_index + 1)]) 
parse (x:y:z:xs) y_index x_index positions -- 3 digit number
    | isDigit x && isDigit y && isDigit z 
    = parse xs y_index (x_index + 3) (positions ++ [(x:[]++y:[]++z:[], y_index, x_index, x_index + 3)])
parse (x:y:xs) y_index x_index positions -- 2 digit number
    | isDigit x && isDigit y 
    = parse xs y_index (x_index + 2) (positions ++ [(x:[]++y:[], y_index, x_index, x_index + 2)]) 
parse (x:xs) y_index x_index positions -- 1 digit number
    | isDigit x 
    = parse xs y_index (x_index + 1) (positions ++ [(x:[], y_index, x_index, x_index + 1)])
parse (x:xs) y_index x_index positions -- Symbol or full stop
    = parse xs y_index (x_index + 1) positions

first_number:: Position -> String
first_number (x, _, _, _) = x

is_number:: Position -> Bool
is_number = isDigit . head . first_number

is_gear:: Position -> Bool
is_gear = (==) '*' . head . first_number

multiply:: (Number, Number) -> Int
multiply = \(a, b) -> (read (first_number a) :: Int) * (read (first_number b) :: Int)

adjacent:: Number -> Gear -> Bool
adjacent (n_val, n_y_index, n_start, n_end) (s_val, s_y_index, s_start, s_end)
    | n_y_index == s_y_index && n_start - 1 <= s_start && n_end + 1 >= s_end = True -- Same
    | n_y_index == s_y_index - 1 && n_start - 1 <= s_start && n_end + 1 >= s_end = True -- Above 
    | n_y_index == s_y_index + 1 && n_start - 1 <= s_start && n_end + 1 >= s_end = True -- Below
    | otherwise = False -- Not adjacent

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

check_all:: [Number] -> [Gear] -> [(Number, Number)]
check_all numbers gears = valid_pairs
    where 
        all_pairs = pairs numbers
        valid_pairs = filter (\(a, b) -> or (map (\g -> adjacent a g && adjacent b g) gears)) all_pairs

get_gears:: (String, YIndex) -> [Gear]
get_gears (str, idx) = gears
    where 
        positions = parse str idx 0 []
        gears = filter is_gear $ positions
        

get_numbers:: (String, YIndex) -> [Number]
get_numbers (str, idx) = numbers
    where 
        positions = parse str idx 0 []
        numbers = (filter is_number) $ positions

format:: String -> [(String, YIndex)]
format x = (\y -> zip y [0..]) $ lines x 

magic:: String -> String
magic x = show $ sum_of_valid
    where 
        numbers = concat $ map get_numbers $ format x
        gears = concat $ map get_gears $ format x
        valid_pairs = check_all numbers gears
        multiply_gears = map multiply valid_pairs
        sum_of_valid = sum multiply_gears

main :: IO () 
main = interact magic