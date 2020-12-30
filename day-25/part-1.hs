solve :: [Int] -> Int
solve [card,door] = (!! loop) (keys door)
    where
        loop = length $ takeWhile (/= card) (keys subject)
        subject = 7
        keys n = iterate ((`mod` 20201227) . (* n)) 1

main :: IO ()
main = interact $ show . solve . map read . lines
