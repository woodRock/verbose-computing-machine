parse :: [String] -> Int
parse grid = length [() | 
    -- Define search directions: (rowStep, colStep, rowStart, rowEnd, colStart, colEnd)
    (di,dj,ri,rf,ci,cf) <- [(0,1,n-1,n-1,0,m-4), -- horizontal
                            (1,0,0,n-4,0,m-1),   -- vertical
                            (1,1,0,n-4,0,m-4),   -- diagonal right
                            (1,-1,0,n-4,3,m-1)], -- diagonal left
    i <- [0..rf], j <- [ci..cf],
    [grid!!(i+k*di)!!(j+k*dj) | k <- [0..3]] `elem` ["XMAS","SAMX"]]
    where (n,m) = (length grid, length $ head grid)

main = interact $ show . parse . lines