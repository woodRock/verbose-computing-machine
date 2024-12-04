parse :: [String] -> Int
parse grid = length [() |
    i <- [0..rows-3], j <- [0..cols-3],
    let diag1 = [grid!!(i+k)!!(j+k) | k <- [0..2]],
    let diag2 = [grid!!(i+k)!!(j+2-k) | k <- [0..2]],
    diag1 `elem` ["MAS","SAM"], diag2 `elem` ["MAS","SAM"]]
  where 
    rows = length grid
    cols = length (head grid)

main = interact $ show . parse . lines