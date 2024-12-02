-- Convert a space-separated string into an integer pair
readPair :: String -> (Int, Int)
readPair = tuple . map read . words
  where tuple [a,b] = (a,b)

-- Calculate similarity score for a pair of lists
similarityScore :: ([Int], [Int]) -> Int
similarityScore (xs, ys) = sum [x * count x ys | x <- xs]
  where count x = length . filter (==x)

main :: IO ()
main = interact $ show . similarityScore . unzip . map readPair . lines