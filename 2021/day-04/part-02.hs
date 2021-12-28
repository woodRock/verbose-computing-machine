import Data.List 
import Data.List.Split

solveOne :: [Int] -> [[Int]] -> (Int,Bool)
solveOne o b = (score, won)
    where 
        score = sum $ concatMap (\\ o) b
        won = any null unmarked -- bingo!
        unmarked = map (\\ o) $ b ++ transpose b

solve :: [Int] -> [[[Int]]] -> Int -> Int
solve o b i
    | last && isWon = n * score 
    | otherwise = solve o gamesInPlay (i + 1) 
    where 
        n = o !! (i - 1) -- index offset 
        last = length b == 1 
        (score, isWon) = head $ map s b
        gamesInPlay = filter ((== False) . snd . s) b
        s = solveOne (take i o)

main = do
    orderLine <- getLine
    cardLines <- getContents
    let order = [read x :: Int | x <- splitOn "," orderLine]
        cardNumbers = [read x :: Int | x <- words cardLines]
        cards = map (chunksOf 5) $ chunksOf (5^2) cardNumbers
    print $ solve order cards 5 -- at least 5 no. needed for bingo. 
