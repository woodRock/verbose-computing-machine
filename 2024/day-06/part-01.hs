import Data.Set (Set)
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Show, Eq)
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
        notInBounds dx dy = dx < 0 || dy < 0 || dx >= length (head maze) || dy >= length maze
        isObstacle dx dy = maze !! dy !! dx == '#'

rotate :: Direction -> Direction
rotate North = East
rotate East  = South
rotate South = West
rotate West  = North

followPath :: Maze -> Int
followPath maze = Set.size visited
    where 
        start = head [(x, y) | y <- [0..h], x <- [0..w], maze !! y !! x == '^']
        h = length maze - 1
        w = length (head maze) - 1
        visited = Set.fromList $ map snd $ 
            takeWhile ((/= (-1, -1)) . snd) $ 
            iterate (\(d, p) -> step d p maze) (North, start)

main :: IO ()
main = interact $ show . followPath . lines