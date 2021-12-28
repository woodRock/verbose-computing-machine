import Data.List 
import Data.List.Split

solveOne :: [Int] -> [[Int]] -> (Int,Bool)
solveOne o b = (score, won)
    where 
        score = sum $ concatMap (\\ o) b
        won = any null unmarked
        unmarked = map (\\ o) $ b ++ transpose b

solve :: [Int] -> [[[Int]]] -> Int -> Int 
solve o b i 
    | length won == 1 = n * score
    | otherwise = solve o b (i + 1)
    where 
        n = o !! (i - 1) -- index offset 
        score = fst $ head won
        won = filter ((== True) . snd) s 
        s = map (solveOne (take i o)) b

main = do
    orderLine <- getLine
    let order = [read x :: Int | x <- splitOn "," orderLine]
    cardLines <- getContents
    let cardNumbers = [read x :: Int | x <- words cardLines]
    let cards = map (chunksOf 5) $ chunksOf 25 cardNumbers
    print $ solve order cards 5 
