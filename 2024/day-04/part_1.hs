import Data.List (transpose)

count :: [String] -> Int
count = length . filter (/= "") . map countOne
    where countOne x = if (x `elem` ["XMAS"]) then x else ""

-- Break into groups of 4.
window :: Int -> String -> [String]
window n [] = []
window n xs = (take n xs) : window 4 (tail xs)

parse :: [String] -> [String]
parse grid = horizontal ++ vertical ++ leftDiagonal ++ rightDiagonal
    where 
        horizontal = grid ++ map reverse grid
        vertical = transpose grid ++ map reverse (transpose grid)
        leftDiagonal = 
            [ [grid!!(i+k)!!(j+k) | k <- [0..3]] | i <- [0..n-4], j <- [0..m-4] ]
            ++ map reverse [[grid!!(i+k)!!(j+k) | k <- [0..3]] | i <- [0..n-4], j <- [0..m-4] ]
        rightDiagonal =
             [ [grid!!(i+k)!!(j-k) | k <- [0..3]] | i <- [0..n-4], j <- [3..m-1] ]
             ++ map reverse [[grid!!(i+k)!!(j-k) | k <- [0..3]] | i <- [0..n-4], j <- [3..m-1] ]
        n = length grid 
        m = length (head grid)

main:: IO ()
main = interact $ show . count . concat . map (window 4) .  parse . lines