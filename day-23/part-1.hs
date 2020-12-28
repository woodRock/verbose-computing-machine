import Data.List (intersperse,maximum,(\\))

parse :: String -> [Int]
parse = map read . words . intersperse ' '

three :: [Int] -> [Int]
three = take 3 . drop 1

dec :: Int -> [Int] -> Int
dec 1 a = maximum (a \\ three a)
dec n a 
    | next `elem` three a = dec next a
    | otherwise = next
    where
        next = n - 1

makeHead :: Int -> [Int] -> [Int]
makeHead i s = take (length s) . dropWhile (/= i) . cycle $ s

turn :: [Int] -> [Int]
turn a = makeHead n res
    where 
        n = a !! 4
        res = head r : three a ++ takeWhile (/= head r) (drop 1 $ cycle r)
        r = makeHead (dec (head a) a) a \\ three a

solve :: [Int] -> String
solve = foldl (\acc i -> acc ++ show i) "" . tail . makeHead 1 . (!! 100) . iterate turn 

main :: IO ()
main = interact $ show . solve . parse
