import Data.List (intersperse,maximum,(\\))

parse :: String -> [Int]
parse = map read . words . intersperse ' '

makeHead :: Int -> [Int] -> [Int]
makeHead i s = take (length s) . dropWhile (/= i) . cycle $ s

three :: [Int] -> [Int]
three = take 3 . drop 1

dec :: Int -> [Int] -> Int
dec 1 a = maximum (a \\ three a)
dec n a 
    | next `elem` three a = dec next a
    | otherwise = next
    where
        next = n - 1

turn :: [Int] -> [Int]
turn a@(x:xs) = makeHead n res
    where 
        n = a !! 4
        res = di : three a ++ takeWhile (/= di) (drop 1 $ cycle rest)
        rest = filter (\x -> x `notElem` three a) (di:r)
        (di:r) = makeHead d a
        d = dec x a 

solve :: [Int] -> String
solve = foldl (\acc i -> acc ++ show i) "" . tail . makeHead 1 . (!! 100) . iterate turn 

main :: IO ()
main = interact $ show . solve . parse
