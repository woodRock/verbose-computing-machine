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
        notInBounds dx dy = dx < 0 || dy < 0 || dx >= length (head maze) || dy >= length maze
        isObstacle dx dy = maze !! dy !! dx == '#'

rotate :: Direction -> Direction
rotate North = East
rotate East  = South
rotate South = West
rotate West  = North

followPath :: Maze -> Bool
followPath maze = isLoop
    where 
        start = head [(x, y) | y <- [0..h], x <- [0..w], maze !! y !! x == '^']
        h = length maze - 1
        w = length (head maze) - 1
        setVisited = Set.fromList visited
        visited = filter ((/= (-1, -1)) . snd) $ take 10000 $ iterate (\(d, p) -> step d p maze) (North, start)
        isLoop = length setVisited /= length visited

placeObstacle :: [Position] -> Maze -> [Maze]
placeObstacle pos maze = [placeObstacleAt x y maze | (x,y) <- pos, maze !! y !! x == '.']
    where 
        placeObstacleAt x y maze = 
            let (h1, h2) = splitAt y maze
                (r1, r2) = splitAt x (head h2)
            in h1 ++ [r1 ++ '#' : tail r2] ++ tail h2

solve :: String -> Int
solve input = length $ filter followPath $ placeObstacle candidatePositions maze
    where 
        maze = lines input
        h = length maze - 1
        w = length (head maze) - 1
        start = head [(x, y) | y <- [0..h], x <- [0..w], maze !! y !! x == '^']
        
        candidatePositions = Set.toList $ Set.fromList $ map snd $ 
            takeWhile ((/= (-1, -1)) . snd) $ 
            iterate (\(d, p) -> step d p maze) (North, start)

main :: IO ()
main = interact $ show . solve