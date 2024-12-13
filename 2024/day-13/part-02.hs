import Text.Read (readMaybe)

type Machine = ((Int,Int), (Int,Int), (Int,Int))  -- ((ax,ay), (bx,by), (px,py))

parseMachine :: [String] -> Maybe Machine
parseMachine [a,b,p] = do
    [ax,ay] <- mapM (readMaybe . drop 2) [init $ words a !! 2, words a !! 3]
    [bx,by] <- mapM (readMaybe . drop 2) [init $ words b !! 2, words b !! 3]
    [px,py] <- mapM (readMaybe . drop 2) [init $ words p !! 1, words p !! 2]
    return ((ax,ay), (bx,by), (10000000000000+px,10000000000000+py))
parseMachine _ = Nothing

parseInput :: String -> [Machine]
parseInput = map (maybe (error "bad input") id . parseMachine) . 
            filter (not . null) . foldr f [[]] . lines
    where f "" acc = []:acc
          f x (y:ys) = (x:y):ys
          f x [] = [[x]]

-- Solve linear equations:
-- ax * a + bx * b = px
-- ay * a + by * b = py
-- Returns token cost if solution exists with non-negative integers
solveLinear :: Machine -> Maybe Int
solveLinear ((ax,ay), (bx,by), (px,py)) = 
    let det = ax*by - ay*bx  -- determinant
    in if det == 0 
       then Nothing  -- no unique solution
       else let a = (px*by - py*bx) `div` det
                b = (ax*py - ay*px) `div` det
            in if a >= 0 && b >= 0 && 
                  a*ax + b*bx == px && a*ay + b*by == py
               then Just (3*a + b)
               else Nothing

main :: IO ()
main = do
    machines <- parseInput <$> getContents
    let tokens = sum . map (maybe 0 id) $ map solveLinear machines
    print tokens