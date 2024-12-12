{-# LANGUAGE BangPatterns #-}
import qualified Data.Array.Unboxed as U
import qualified Data.Array.ST as ST
import Data.Array.Unboxed (UArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when)
import Data.STRef

type Pos = (Int, Int)
type Grid = UArray Pos Char

parseGrid :: [String] -> Grid
parseGrid rows = 
    let height = length rows
        width = length (head rows)
    in U.array ((0,0), (width-1, height-1))
        [((x,y), c) | (y, row) <- zip [0..] rows, 
                      (x, c) <- zip [0..] row]

countBoundaries :: Grid -> [(Int, Int)] -> Char -> Int
countBoundaries grid points c = sum $ map checkCell points
  where
    bounds = U.bounds grid
    pointSet = [(x,y) | (x,y) <- points]  -- for checking membership
    checkCell (x,y) = sum [
        if inRegion (x+1,y) then 0 else 1,
        if inRegion (x-1,y) then 0 else 1,
        if inRegion (x,y+1) then 0 else 1,
        if inRegion (x,y-1) then 0 else 1]
    inRegion pos = pos `elem` pointSet

findRegions :: Grid -> [(Char, Int, Int)]
findRegions grid = runST $ do
    let bounds@((minX,minY),(maxX,maxY)) = U.bounds grid
    visited <- ST.newArray bounds False :: ST s (ST.STUArray s Pos Bool)
    regions <- newSTRef []
    
    forM_ [(x,y) | y <- [minY..maxY], x <- [minX..maxX]] $ \pos -> do
        isVisited <- ST.readArray visited pos
        when (not isVisited) $ do
            let c = grid U.! pos
            points <- floodFill visited pos c
            when (not $ null points) $ do
                let size = length points
                    perim = countBoundaries grid points c
                modifySTRef' regions ((c, size, perim):)
    
    readSTRef regions
  where
    bounds = U.bounds grid
    floodFill visited start c = do
        positions <- newSTRef []
        let go [] = return ()
            go (p:ps) 
                | not (U.inRange bounds p) = go ps
                | otherwise = do
                    v <- ST.readArray visited p
                    if v 
                        then go ps
                        else do
                            when (grid U.! p == c) $ do
                                ST.writeArray visited p True
                                modifySTRef' positions (p:)
                                let ns = [(fst p + dx, snd p + dy) | 
                                        (dx, dy) <- [(1,0), (-1,0), (0,1), (0,-1)]]
                                go (ns ++ ps)
                            go ps
        go [start]
        readSTRef positions

solve :: [String] -> Int
solve input = 
    let grid = parseGrid input
        regions = findRegions grid
    in sum [size * perim | (_, size, perim) <- regions]

main :: IO ()
main = interact $ show . solve . lines