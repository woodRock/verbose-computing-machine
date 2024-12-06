import Data.Set (Set)
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Eq, Ord, Show)
data Position = Position { x :: Int, y :: Int } deriving (Eq, Ord, Show)
data Guard = Guard { pos :: Position, dir :: Direction } deriving (Show, Eq, Ord)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

forward :: Guard -> Position
forward (Guard (Position x y) dir) = case dir of
    North -> Position x (y-1)
    East  -> Position (x+1) y
    South -> Position x (y+1)
    West  -> Position (x-1) y

inBounds :: Int -> Int -> Position -> Bool
inBounds width height (Position x y) = 
    x >= 0 && x < width && y >= 0 && y < height

parseGrid :: String -> (Guard, Set Position)
parseGrid input = (guard, obstacles)
  where
    rows = lines input
    positions = [(x, y, c) | (y, row) <- zip [0..] rows, 
                            (x, c) <- zip [0..] row]
    guard = head [Guard (Position x y) North | (x, y, c) <- positions, c == '^']
    obstacles = Set.fromList [Position x y | (x, y, c) <- positions, c == '#']

-- Get full path of guard with set of states and obstacles visited
simulateGuard :: Int -> Int -> Set Position -> Guard -> (Set Guard, Set Position)
simulateGuard width height obstacles initialGuard = go Set.empty Set.empty initialGuard
  where
    go states positions guard
        | not (inBounds width height nextPos) = (states, positions)
        | Set.member guard states = (states, positions)
        | Set.member nextPos obstacles = 
            go (Set.insert guard states) 
               (Set.insert nextPos positions)
               (Guard (pos guard) (turnRight (dir guard)))
        | otherwise = 
            go (Set.insert guard states)
               (Set.insert nextPos positions)
               (Guard nextPos (dir guard))
      where
        nextPos = forward guard

-- Check if position creates a loop
createsLoop :: Int -> Int -> Set Position -> Set Guard -> Guard -> Position -> Bool
createsLoop width height obstacles originalStates initialGuard testPos = go Set.empty initialGuard
  where
    go visited guard
        | not (inBounds width height nextPos) = False
        | Set.member guard visited = True
        | nextPos == testPos || Set.member nextPos obstacles =
            go (Set.insert guard visited) (Guard (pos guard) (turnRight (dir guard)))
        | otherwise =
            go (Set.insert guard visited) (Guard nextPos (dir guard))
      where
        nextPos = forward guard

solve2 :: String -> Int
solve2 input = length loopPositions
  where
    rows = lines input
    width = length (head rows)
    height = length rows
    (initialGuard, obstacles) = parseGrid input
    
    -- Get original path
    (originalStates, visitedPositions) = simulateGuard width height obstacles initialGuard
    startingPos = pos initialGuard
    
    -- Get candidate positions
    candidates = [ nextPos | guard <- Set.toList originalStates
                         , let nextPos = forward guard
                         , inBounds width height nextPos
                         , not (Set.member nextPos obstacles)
                         , nextPos /= startingPos
                         ]
    
    -- Test each position
    loopPositions = filter (createsLoop width height obstacles originalStates initialGuard) 
                    (Set.toList $ Set.fromList candidates)

main :: IO ()
main = interact $ show . solve2