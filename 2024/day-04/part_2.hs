-- Intersecting mas or sam diagonally.
parse :: [String] -> Int
parse grid = length [() | i <- [0..n-3], j <- [0..m-3],
    let diag1 = [grid!!(i+k)!!(j+k) | k <- [0..2]],
    let diag2 = [grid!!(i+k)!!(j+2-k) | k <- [0..2]],
    diag1 `elem` ["MAS","SAM"], diag2 `elem` ["MAS","SAM"]]
    where 
        n = length grid 
        m = length (head grid)

main:: IO ()
main = interact $ show .  parse . lines