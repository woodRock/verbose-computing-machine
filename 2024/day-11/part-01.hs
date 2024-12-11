rules :: Int -> [Int] 
rules 0 = [1]
rules n 
  | even (length $ show n) =
    let s = show n
        (l, r) = splitAt (length s `div` 2) s
     in map read [l, r]
  | otherwise = [n * 2024]

blink :: [Int] -> [Int]
blink x = concatMap (\n -> rules n) x

main :: IO () 
main = interact $ show . length . last . take (25 + 1). iterate blink . map read . words