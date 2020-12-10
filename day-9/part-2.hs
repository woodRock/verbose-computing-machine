type Index = Int
type Sum = Int 
type Series = [Int]
type Preamble = Int
type Pair = (Int,Int)
type Set = [Int]

slice :: Index -> Index -> Series -> Series
slice x y = take (y - x + 1) . drop x

takeLast :: Index -> Preamble -> Series -> Series
takeLast i n = slice (i - n) (i - 1) 

contiguous :: Series -> [Set]
contiguous a = map (\(x,y) -> slice x y a) [(x,y) | x <- [0..n], y <- [0..n], x < y]
    where n = length a - 1

pairs :: Sum -> Series -> [Pair]
pairs z a = [(a !! x, a !! y) | x <- [0..n], y <- [0..n], x < y, a !! x + a !! y == z]
    where n = length a - 1

solve :: Preamble -> Series -> Sum 
solve n a = maximum cs + minimum cs
    where
        cs = head $ filter ((== a !! idx). sum) $ contiguous $ slice 0 (idx - 1) a  
        (idx, _) = head $ filter (null . snd) $ 
            map (\i -> (i, pairs (a !! i) $ takeLast i n a)) 
            [n + 1 .. length a - 1] 

main :: IO ()
main = interact $ show . solve n . map read . lines
    where n = 5
