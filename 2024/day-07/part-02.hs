import Data.List (foldl')

parse :: String -> (Int, [Int])
parse s = (read v, map read $ words r)
    where (v, _:r) = span (/= ':') s

ops :: [Int -> Int -> Int]
ops = [(+), (*), \x y -> read $ show x ++ show y]

solve :: (Int, [Int]) -> [Int]
solve (v, []) = []
solve (v, [x]) = [v | v == x]
solve (v, ts) = [v | v `elem` results]
    where
        opCombinations = sequence $ replicate (length ts - 1) [0..2]
        evaluate ops' = foldl' (\acc (op, n) -> (ops !! op) acc n) 
                              (head ts) 
                              (zip ops' (tail ts))
        results = map evaluate opCombinations

main :: IO ()
main = interact $ show . sum . concatMap (solve . parse) . lines