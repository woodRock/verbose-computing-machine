import Data.List (groupBy)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

type Position = ([Char], (Char, Int, Int))

-- check :: Position -> Position -> Bool
-- check (n_str, (n_s_c, n_s, n_e)) (s_str, (s_s_c, s_s, s_e)) 
--     | 

-- solve :: String -> [(Char, Int, Int)]
-- solve :: String -> [String]
solve:: String -> [(Position)]
solve x = symbols ++ []
    where 
        l = length x
        chars = zip x [0..l]
        chars_by_index = groupBy (\a b -> fst a /= '.' && fst b /= '.') chars
        remove_dots = filter (\x -> fst (head x) /= '.') chars_by_index
        chars_by_idx = map (\x -> (fst (head x), snd (head x), snd (last x))) remove_dots
        concat_chars = map (\x -> map (\y -> fst y) x) remove_dots
        results = zip concat_chars chars_by_idx
        numbers = filter (\x -> isDigit (head $ fst x)) results
        symbols = filter (\x -> not (isDigit (head $ fst x))) results
        -- Same line - not relevant!
        same = zip numbers symbols
        -- Touching 
        symbol_is_after = filter (\x -> not (isDigit (last $ fst x))) numbers
        -- Symbol is before
        

main :: IO () 
main = interact $ show . map solve . lines