import Data.List (intersperse,maximum,(\\))

parse :: String -> [Int]
parse = map read . words . intersperse ' '

three :: [Int] -> [Int]
three = take 3 . drop 1

dec :: Int -> [Int] -> Int
dec 1 a = maximum (a \\ three a)
dec n a 
    | (n - 1) `elem` three a = dec (n - 1) a
    | otherwise = n - 1

makeHead :: Int -> [Int] -> [Int]
makeHead i a = take (length a) . dropWhile (/= i) . cycle $ a

turn :: [Int] -> [Int]
turn a = makeHead (a !! 4) res
    where 
        res = head r : three a ++ takeWhile (/= head r) (drop 1 $ cycle r)
        r = makeHead (dec (head a) a) a \\ three a

solve :: [Int] -> String
solve = foldl (\acc i -> acc ++ show i) "" . tail . makeHead 1 . (!! 100) . iterate turn 

main :: IO ()
main = interact $ show . solve . parse
