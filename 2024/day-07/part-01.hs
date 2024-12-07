parse :: String -> (Int, [Int])
parse s = (read v, map read $ words r)
    where (v, _:r) = span (/= ':') s

solve :: (Int, [Int]) -> [Int]
solve (v, ts) = [v | v `elem` results]
    where
        opCombinations = sequence $ replicate (length ts - 1) [(+), (*)]
        evaluate ops = foldl (\a (f,n) -> f a n) (head ts) (zip ops (tail ts))
        results = map evaluate opCombinations

main :: IO ()
main = interact $ show . sum . concatMap (solve . parse) . lines