import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type Position = (Int, Int)  -- (col, row) or (x,y)
data Cell = Wall | Empty | Visited deriving (Eq)
type Maze = [[Cell]]

instance Show Cell where
    show Wall = "#"
    show Empty = "."
    show Visited = "O"

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn sep [] = []
splitOn sep xs = 
    let (chunk, rest) = break (== head sep) xs
    in case rest of
        [] -> [chunk]
        (_:rest') -> if isPrefixOf sep rest
                     then chunk : splitOn sep (drop (length sep) rest)
                     else chunk : splitOn sep rest'
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

parse :: String -> Position
parse = (\[a,b] -> (a,b)) . map read . splitOn ","

djikstra :: Maze -> Position -> Position -> [Position]
djikstra maze start end = 
    let
        (rows, cols) = (length maze, length (head maze))
        
        -- Helper function to get valid neighbors
        getNeighbors (col, row) = 
            filter isValid [(col, row+1), (col, row-1), (col+1, row), (col-1, row)]
            where
                isValid (c, r) = c >= 0 && c < cols && 
                                r >= 0 && r < rows && 
                                maze !! r !! c /= Wall
        
        -- Main search function
        search :: Set Position -> Map Position Position -> [(Position, Int)] -> Maybe [Position]
        search visited prev [] = Nothing
        search visited prev ((pos, dist):queue)
            | pos == end = Just $ reconstructPath prev end
            | Set.member pos visited = search visited prev queue
            | otherwise = 
                let neighbors = filter (not . (`Set.member` visited)) $ getNeighbors pos
                    newQueue = queue ++ map (\p -> (p, dist + 1)) neighbors
                    newPrev = foldr (\p m -> Map.insert p pos m) prev neighbors
                in search (Set.insert pos visited) newPrev newQueue
        
        -- Helper function to reconstruct the path
        reconstructPath :: Map Position Position -> Position -> [Position]
        reconstructPath prev pos =
            case Map.lookup pos prev of
                Nothing -> [pos]
                Just prevPos -> reconstructPath prev prevPos ++ [pos]
                
    in case search Set.empty Map.empty [(start, 0)] of
        Nothing -> []  -- No path found
        Just path -> path

solve :: [Position] -> Int 
solve walls = length $  djikstra maze start end
    where 
        maze = [[if (col, row) `elem` walls then Wall else Empty | col <- [0..70]] | row <- [0..70]]
        start = (0,0)
        end = (70,70)

printMaze :: (Maze, [Position]) -> String
printMaze (maze, visited) = unlines $ map (concatMap show) $ zipWith (\row y -> 
    zipWith (\cell x -> if (x,y) `elem` visited then Visited else cell) row [0..]) maze [0..]

main :: IO () 
main = interact $ show .  solve . take 1024 . map parse . lines