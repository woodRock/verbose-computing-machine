import Data.Maybe

pairs :: [Int] -> [(Int,Int)]
pairs a = [(a !! x, a !! y) | x <- [0..n], y <- [0..n], x < y]
    where n = length a - 1

check :: Int -> [(Int,Int)] -> Bool
check z = any ((== z). uncurry (+)) 

slice :: Int -> Int -> [Int] -> [Int]
slice x y = take (y - x + 1) . drop x

takeLast :: Int -> Int -> [Int] -> [Int]
takeLast i n = slice (i - n) (i - 1) 

solve :: Int -> [Int] -> Int 
solve n a = fst $ head $ filter ((== False) . snd) $ 
            map (\i -> (a !! i, check (a !! i) $ pairs $ takeLast i n a)) 
            [n + 1 .. length a - 1] 

main :: IO ()
main = interact $ show . solve n . map read . lines
    where n = 25
