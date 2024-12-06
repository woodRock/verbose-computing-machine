import Data.Set (Set)
import qualified Data.Set as Set

-- Data types for direction and position
data Direction = North | East | South | West deriving (Eq, Show)
data Position = Position { x :: Int, y :: Int } deriving (Eq, Ord, Show)
data Guard = Guard { pos :: Position, dir :: Direction } deriving (Show)

-- Parse the input grid
parseGrid :: String -> (Guard, Set Position)
parseGrid input = (guard, obstacles)
  where
    rows = lines input
    height = length rows
    width = length (head rows)
    
    positions = [(x, y, c) | (y, row) <- zip [0..] rows, 
                            (x, c) <- zip [0..] row]
    
    guard = head [Guard (Position x y) North | (x, y, c) <- positions, c == '^']
    obstacles = Set.fromList [Position x y | (x, y, c) <- positions, c == '#']

-- Turn right
turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

-- Get position in front
forward :: Guard -> Position
forward (Guard (Position x y) dir) = case dir of
    North -> Position x (y-1)
    East  -> Position (x+1) y
    South -> Position x (y+1)
    West  -> Position (x-1) y

-- Check if position is within grid
inBounds :: Int -> Int -> Position -> Bool
inBounds width height (Position x y) = 
    x >= 0 && x < width && y >= 0 && y < height

-- Move guard according to rules
move :: Int -> Int -> Set Position -> Guard -> Maybe Guard
move width height obstacles guard = 
    if not (inBounds width height (pos guard))
        then Nothing
        else Just $ if Set.member frontPos obstacles
            then Guard (pos guard) (turnRight (dir guard))
            else Guard frontPos (dir guard)
  where
    frontPos = forward guard

-- Get all positions visited by guard
visitedPositions :: Int -> Int -> Set Position -> Guard -> Set Position
visitedPositions width height obstacles = go Set.empty
  where
    go visited guard = case move width height obstacles guard of
        Nothing -> visited
        Just newGuard -> go (Set.insert (pos guard) visited) newGuard

-- Main solution
solve :: String -> Int
solve input = Set.size visited
  where
    rows = lines input
    width = length (head rows)
    height = length rows
    (guard, obstacles) = parseGrid input
    visited = visitedPositions width height obstacles guard

main :: IO ()
main = interact $ show . solve