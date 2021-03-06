type Pair = (Int, Int)
type Index = Int
type Sum = Int
type Preamble = Int
type Series = [Int]
type Invalid = Int

pairs :: Sum -> Series -> [Pair]
pairs z a = [(a !! x, a !! y) | x <- [0..n], y <- [0..n], x < y, a !! x + a !! y == z]
    where n = length a - 1

slice :: Index -> Index -> Series -> Series
slice x y = take (y - x + 1) . drop x

takeLast :: Index -> Preamble -> Series -> Series
takeLast i n = slice (i - n) (i - 1) 

solve :: Preamble -> Series -> Invalid 
solve n a = fst $ head $ filter (null . snd) $ 
            map (\i -> (a !! i, pairs (a !! i) $ takeLast i n a)) 
            [n + 1 .. length a - 1] 

main :: IO ()
main = interact $ show . solve n . map read . lines
    where n = 5

