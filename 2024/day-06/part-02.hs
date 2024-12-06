import Data.Set (Set)
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Show, Eq, Ord)
type Position = (Int, Int)
type Maze = [String]

step :: Direction -> Position -> Maze -> (Direction, Position)
step dir (x, y) maze 
    | notInBounds x' y' = (dir, (-1, -1))
    | isObstacle x' y' = (rotate dir, (x, y))
    | otherwise = (dir, (x', y'))
    where 
        (x', y') = case dir of
            North -> (x, y - 1)
            East  -> (x + 1, y)
            South -> (x, y + 1)
            West  -> (x - 1, y)
        notInBounds dx dy = dx < 0 || dy < 0 || 
                           dx >= length (head maze) || dy >= length maze
        isObstacle dx dy = maze !! dy !! dx == '#'

rotate :: Direction -> Direction
rotate North = East
rotate East = South
rotate South = West
rotate West = North

isLoop :: Maze -> Bool
isLoop maze = length path /= length (Set.fromList path)
    where 
        w = length (head maze) - 1
        h = length maze - 1
        start = head [(x, y) | y <- [0..h], x <- [0..w], maze !! y !! x == '^']
        path = filter ((/= (-1, -1)) . snd) $ 
               take 10000 $ 
               iterate (\(d, p) -> step d p maze) (North, start)

addObstacle :: Position -> Maze -> Maze
addObstacle (x, y) maze = 
    take y maze ++ 
    [take x row ++ '#' : drop (x + 1) row] ++ 
    drop (y + 1) maze
    where row = maze !! y

solve :: String -> Int
solve input = length $ filter isLoop $ map (`addObstacle` maze) candidates
    where 
        maze = lines input
        w = length (head maze) - 1
        h = length maze - 1
        start = head [(x, y) | y <- [0..h], x <- [0..w], maze !! y !! x == '^']
        candidates = filter (\p -> maze !! snd p !! fst p == '.') $
                    Set.toList $ Set.fromList $ map snd $ 
                    takeWhile ((/= (-1, -1)) . snd) $ 
                    iterate (\(d, p) -> step d p maze) (North, start)

main :: IO ()
main = interact $ show . solve