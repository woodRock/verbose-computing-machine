rules :: Int -> [Int] 
rules 0 = [1] -- If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
rules n -- If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
  | even (length $ show n) =
    let s = show n
        (l, r) = splitAt (length s `div` 2) s
     in map read [l, r]
  | otherwise = [n * 2024] -- If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.  

blink :: [Int] -> [Int]
blink x = concatMap (\n -> rules n) x

main :: IO () 
main = interact $ show . length . last . take n . iterate blink . map read . words
  where n = 75 + 1