{-# LANGUAGE BangPatterns #-}
import qualified Data.Array.Unboxed as U
import qualified Data.Array.ST as ST
import Data.Array.Unboxed (UArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when)
import Data.STRef
import qualified Data.Map.Strict as Map

type Pos = (Int, Int)
type Grid = UArray Pos Char
type CCGrid = UArray Pos Int

parseGrid :: [String] -> Grid
parseGrid rows = 
    let height = length rows
        width = length (head rows)
    in U.array ((0,0), (width-1, height-1))
        [((x,y), c) | (y, row) <- zip [0..] rows, 
                      (x, c) <- zip [0..] row]

findComponents :: Grid -> (CCGrid, Int)
findComponents grid = runST $ do
    let bounds@((minX,minY),(maxX,maxY)) = U.bounds grid
    cc <- ST.newArray bounds (-1) :: ST s (ST.STUArray s Pos Int)
    numccRef <- newSTRef 0
    
    let dfs x y ccNum = do
            let val = grid U.! (x,y)
            curCC <- ST.readArray cc (x,y)
            when (curCC == -1) $ do
                ST.writeArray cc (x,y) ccNum
                -- Check all four directions
                when (x > minX && grid U.! (x-1,y) == val) $ 
                    dfs (x-1) y ccNum
                when (x < maxX && grid U.! (x+1,y) == val) $ 
                    dfs (x+1) y ccNum
                when (y > minY && grid U.! (x,y-1) == val) $ 
                    dfs x (y-1) ccNum
                when (y < maxY && grid U.! (x,y+1) == val) $ 
                    dfs x (y+1) ccNum
    
    forM_ [(x,y) | y <- [minY..maxY], x <- [minX..maxX]] $ \pos -> do
        curCC <- ST.readArray cc pos
        when (curCC == -1) $ do
            ccNum <- readSTRef numccRef
            dfs (fst pos) (snd pos) ccNum
            modifySTRef' numccRef (+1)
    
    finalCC <- ST.freeze cc
    numcc <- readSTRef numccRef
    return (finalCC, numcc)

calculateMetrics :: CCGrid -> (Map.Map Int Int, Map.Map Int Int, Map.Map Int Int)
calculateMetrics cc = 
    let bounds@((minX,minY),(maxX,maxY)) = U.bounds cc
        
        get x y | x < minX || x > maxX || y < minY || y > maxY = -1
                | otherwise = cc U.! (x,y)
        
        -- Calculate corners
        cornerPoints = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
        corners = foldr countCorners Map.empty cornerPoints
        
        -- Calculate area and perimeter
        points = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
        (area, perim) = foldr countAreaAndPerim (Map.empty, Map.empty) points
        
        countCorners (x,y) m =
            let a = get x y
                diagonals = [((x+dx,y+dy), (x+dx,y), (x,y+dy)) 
                            | (dx,dy) <- [(-1,-1),(-1,1),(1,-1),(1,1)]]
                cornerCount = sum [1 | (p1,p2,p3) <- diagonals,
                                 let v1 = get (fst p1) (snd p1)
                                     v2 = get (fst p2) (snd p2)
                                     v3 = get (fst p3) (snd p3),
                                 (a /= v2 && a /= v3) || (a == v2 && a == v3 && a /= v1)]
            in Map.insertWith (+) a cornerCount m
            
        countAreaAndPerim (x,y) (am, pm) =
            let a = get x y
                k = 4 - (if get x (y-1) == a then 2 else 0) 
                      - (if get (x-1) y == a then 2 else 0)
            in (Map.insertWith (+) a 1 am, Map.insertWith (+) a k pm)
            
    in (area, perim, corners)

solve :: [String] -> (Int, Int)
solve input = 
    let grid = parseGrid input
        (cc, numcc) = findComponents grid
        (area, perim, corners) = calculateMetrics cc
        part1 = sum [a * p | (k,a) <- Map.toList area, 
                            let p = Map.findWithDefault 0 k perim]
        part2 = sum [a * c | (k,a) <- Map.toList area,
                            let c = Map.findWithDefault 0 k corners]
    in (part1, part2)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let (part1, part2) = solve input
    putStrLn $ show part1 ++ " " ++ show part2