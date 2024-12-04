import Data.Char (toLower)

parse :: [String] -> Int
parse grid = length [() | 
   (di,dj,ri,rf,ci,cf) <- [(0,1,n-1,n-1,0,m-4), (1,0,0,n-4,0,m-1), 
                           (1,1,0,n-4,0,m-4), (1,-1,0,n-4,3,m-1)],
   i <- [0..rf], j <- [ci..cf],
   [grid!!(i+k*di)!!(j+k*dj) | k <- [0..3]] `elem` ["XMAS","SAMX"]]
 where (n,m) = (length grid, length $ head grid)

main = interact $ show . parse . lines