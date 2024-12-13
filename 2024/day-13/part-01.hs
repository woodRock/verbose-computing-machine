import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)

type Machine = ((Int,Int), (Int,Int), (Int,Int))  -- ((ax,ay), (bx,by), (px,py))

parseMachine :: [String] -> Maybe Machine
parseMachine [a,b,p] = do
    [ax,ay] <- mapM (readMaybe . drop 2) [init $ words a !! 2, words a !! 3]
    [bx,by] <- mapM (readMaybe . drop 2) [init $ words b !! 2, words b !! 3]
    [px,py] <- mapM (readMaybe . drop 2) [init $ words p !! 1, words p !! 2]
    return ((ax,ay), (bx,by), (px,py))
parseMachine _ = Nothing

parseInput :: String -> [Machine]
parseInput = map (maybe (error "bad input") id . parseMachine) . 
            filter (not . null) . foldr f [[]] . lines
    where f "" acc = []:acc
          f x (y:ys) = (x:y):ys
          f x [] = [[x]]

solve :: Machine -> Int -> Maybe Int
solve ((ax,ay), (bx,by), (px,py)) limit = 
    listToMaybe [3*a + b | 
        a <- [0..limit], b <- [0..limit],
        a*ax + b*bx == px, a*ay + b*by == py]

main :: IO ()
main = do
    machines <- parseInput <$> getContents
    let tokens = sum . map (maybe 0 id) $ map (`solve` 100) machines
    print tokens