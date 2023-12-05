import Data.Char
import Data.Text (replace, pack, unpack)

replace_words_with_nums:: String -> String
replace_words_with_nums x = 
    foldl (\x (s,s') -> 
        unpack $ replace (pack s) (pack s') (pack x)) 
    x number_strings_dict
    where 
        number_strings_dict = [
            ("one","one1one"),
            ("two","two2two"),
            ("three","three3three"),
            ("four","four4four"),
            ("five","five5five"),
            ("six","six6six"),
            ("seven","seven7seven"),
            ("eight","eight8eight"),
            ("nine","nine9nine")
         ]

solve :: String -> Int 
solve x = first_digit * 10 + last_digit
    where 
        number_strings = replace_words_with_nums x 
        numbers_only = 
            map (\x -> read [x]::Int) 
            (filter (\x -> isDigit x) number_strings) 
        first_digit = head numbers_only 
        last_digit = last numbers_only 

main :: IO () 
main = interact $ show . sum . map solve . lines