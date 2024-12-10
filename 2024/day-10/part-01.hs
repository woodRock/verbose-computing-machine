import Data.Char (digitToInt)
import qualified Data.Set as Set
import Data.List (nub)

findStarts :: [String] -> [(Int, Int)]
findStarts grid = [(x,y) | (y, row) <- zip [0..] grid, (x, c) <- zip [0..] row, c == '0']

score :: [[[(Int, Int, Int)]]] -> Int 
score paths = sum $ map (\th -> length $ nub $ [(x,y) | (x,y,h) <- concat th, h == 9]) paths

walkPath :: (Int, Int) -> Set.Set (Int, Int) -> [[Int]] -> [[(Int, Int, Int)]]
walkPath c@(x,y) visited heights 
  | isSummit = [[(x,y, 9)]]
  | null nextPaths = []
  | otherwise = allPaths 
  where 
    isSummit = height == 9
    height = heights !! y !! x
    up = (x, y-1)
    left = (x-1, y)
    down = (x, y+1)
    right = (x+1,y)
    nextPaths = filter (\p -> inBounds p && smooth p && not (hasVisited p)) 
                      [up, left, right, down]
    allPaths = map ((x,y, height) :) $
               concatMap (\next -> walkPath next (Set.insert (x,y) visited) heights) nextPaths
    inBounds (px,py) = px >= 0 && py >= 0 && px < w && py < h
    w = length $ head heights 
    h = length heights
    smooth (px,py) = ((heights !! py !! px) - height) == 1
    hasVisited p = Set.member p visited

path :: [String] -> [[[(Int, Int, Int)]]]
path x = map (\s -> walkPath s Set.empty heights) starts
  where 
    heights = map (map digitToInt) x
    starts = findStarts x

main :: IO () 
main = interact $ show . score . path . lines 