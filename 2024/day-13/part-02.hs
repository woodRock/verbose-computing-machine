import Text.Read (readMaybe)

type Matrix = [[Int]]
type Vector = [Int]
type Machine = ((Int,Int), (Int,Int), (Int,Int))  -- ((ax,ay), (bx,by), (px,py))

-- 2x2 matrix determinant
determinant :: Matrix -> Int
determinant [[a,b],[c,d]] = a*d - b*c

-- 2x2 matrix inverse multiplied by determinant to keep integers
-- Returns (matrix * det, det)
matrixInverse :: Matrix -> (Matrix, Int)
matrixInverse [[a,b],[c,d]] = 
    let det = determinant [[a,b],[c,d]]
    in ([[d,-b],[-c,a]], det)

-- Matrix-vector multiplication
matrixVectorMul :: Matrix -> Vector -> Vector
matrixVectorMul m v = map (sum . zipWith (*) v) m

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

-- Solve using matrix multiplication:
-- [ax bx] [a] = [px]
-- [ay by] [b]   [py]
solveMatrix :: Machine -> Maybe Int
solveMatrix ((ax,ay), (bx,by), (px,py)) = 
    let matrix = [[ax,bx],[ay,by]]
        vector = [px,py]
        (inv, det) = matrixInverse matrix
        -- Solution = inverse * vector / det
        [a,b] = map (`div` det) $ matrixVectorMul inv vector
    in if det == 0 || 
          any ((/= 0) . (`mod` det)) (matrixVectorMul inv vector) ||
          a < 0 || b < 0 
       then Nothing
       else if a*ax + b*bx == px && a*ay + b*by == py
            then Just (3*a + b)
            else Nothing

main :: IO ()
main = do
    machines <- parseInput <$> getContents
    let tokens = sum . map (maybe 0 id) $ map solveMatrix machines
    print tokens