import Data.Char (isDigit)

type Value = String 
type YIndex = Int
type XIndex = Int
type Start = XIndex
type End = XIndex
type Position = (Value, YIndex, Start, End)
type Number = Position
type Symbol = Position

parse :: String -> YIndex -> XIndex -> [Position] -> [Position]
parse [] y_index x_index positions = positions
parse ('.':xs) y_index x_index positions -- Full stop
    = parse xs y_index (x_index + 1) positions
parse (x:y:z:xs) y_index x_index positions -- 3 digit number
    | isDigit x && isDigit y && isDigit z 
    = parse xs y_index (x_index + 3) (positions ++ [(x:[]++y:[]++z:[], y_index, x_index, x_index + 3)])
parse (x:y:xs) y_index x_index positions -- 2 digit number
    | isDigit x && isDigit y 
    = parse xs y_index (x_index + 2) (positions ++ [(x:[]++y:[], y_index, x_index, x_index + 2)]) 
parse (x:xs) y_index x_index positions -- 1 digit number
    | isDigit 
    x = parse xs y_index (x_index + 1) (positions ++ [(x:[], y_index, x_index, x_index + 1)])
parse (x:xs) y_index x_index positions -- Symbol
    = parse xs y_index (x_index + 1) (positions ++ [(x:[], y_index, x_index, x_index + 1)]) 

check:: Number -> Symbol -> Bool
check (n_val, n_y_index, n_start, n_end) (s_val, s_y_index, s_start, s_end)
    | n_y_index == s_y_index && n_start - 1 <= s_start && n_end + 1 >= s_end = True
    | n_y_index == s_y_index - 1 && n_start - 1 <= s_start && n_end + 1 >= s_end = True -- Above 
    | n_y_index == s_y_index + 1 && n_start - 1 <= s_start && n_end + 1 >= s_end = True -- Below
    | otherwise = False

check_all:: [Number] -> [Symbol] -> [Bool]
check_all numbers symbols = (map (\n -> or (map (\s -> check n s) symbols)) numbers)
    
get_symbols:: (String, YIndex) -> [Symbol]
get_symbols (str, idx) = symbols
    where 
        positions = parse str idx 0 []
        symbols = filter is_symbol $ positions
        is_number = isDigit . head . first_number
        first_number (x, _, _, _) = x
        is_symbol = not . is_number

get_numbers:: (String, YIndex) -> [Number]
get_numbers (str, idx) = numbers
    where 
        positions = parse str idx 0 []
        numbers = (filter is_number) $ positions
        is_number = isDigit . head . first_number
        first_number (x, _, _, _) = x

format:: String -> [(String, YIndex)]
format x = (\y -> zip y [0..]) $ lines x 

magic:: String -> String
magic x = show sum_of_valid
    where 
        numbers = concat $ map get_numbers $ format x
        symbols = concat $ map get_symbols $ format x
        results = check_all numbers symbols
        valid_numbers = map first_number $ map fst $ filter snd $ zip numbers results :: [String]
        first_number (x, _, _, _) = x :: String
        sum_of_valid = sum $ map (\x -> read x :: Int) valid_numbers :: Int

main :: IO () 
main = interact magic