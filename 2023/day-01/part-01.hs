import Data.Char

solve :: String -> Int 
solve x = first_digit * 10 + last_digit
    where 
        numbers_only = 
            map (\x -> read [x]::Int) 
            (filter (\x -> isDigit x) x) 
        first_digit = head numbers_only 
        last_digit = last numbers_only 

main :: IO () 
main = interact $ show . sum . map solve . lines