import Data.List (find)

data Direction = North | East | South | West deriving (Eq, Show)
type Position = (Int, Int)  -- (row, col)
type State = (Position, Direction)
type Grid = [(Position, Char)]
type Cost = Int

-- Parse the input grid
parseGrid :: String -> Grid
parseGrid input = [((i, j), c)
                 | (i, row) <- zip [0..] (lines input)
                 , (j, c) <- zip [0..] row]

-- Find positions with specific character
findChar :: Char -> Grid -> Maybe Position
findChar target = fmap fst . find ((== target) . snd)

-- Get the new position after moving one step in current direction
move :: Direction -> Position -> Position
move North (r, c) = (r-1, c)
move South (r, c) = (r+1, c)
move East  (r, c) = (r, c+1)
move West  (r, c) = (r, c-1)

-- Lookup a position in the grid
gridLookup :: Position -> Grid -> Maybe Char
gridLookup pos = fmap snd . find ((== pos) . fst)

-- Get next possible states and their costs
nextStates :: Grid -> State -> [(State, Cost)]
nextStates grid (pos, dir) = 
    forward ++ turns
  where
    forward = case gridLookup (move dir pos) grid of
        Just c | c /= '#' -> [((move dir pos, dir), 1)]
        _ -> []
    turns = [(turn pos dir', 1000) | dir' <- turnDirections dir]
    turn pos dir = (pos, dir)
    turnDirections North = [West, East]
    turnDirections South = [East, West]
    turnDirections East = [North, South]
    turnDirections West = [South, North]

-- Manhattan distance heuristic
heuristic :: Position -> Position -> Int
heuristic (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

-- Priority queue implementation using sorted list
type PQueue = [(Int, (Cost, State))]

pqEmpty :: PQueue
pqEmpty = []

pqInsert :: Int -> (Cost, State) -> PQueue -> PQueue
pqInsert priority item [] = [(priority, item)]
pqInsert priority item ((p, x):xs)
    | priority <= p = (priority, item) : (p, x) : xs
    | otherwise = (p, x) : pqInsert priority item xs

-- A* search implementation
astar :: Grid -> State -> Position -> Maybe Cost
astar grid start end = search [] [(0, (0, start))]
  where
    search visited [] = Nothing
    search visited ((_, (cost, state@(pos, _))):rest)
        | pos == end = Just cost
        | state `elem` visited = search visited rest
        | otherwise = search (state:visited) queue'
      where
        nexts = nextStates grid state
        queue' = foldr insertNext rest nexts
        insertNext (s@(p, _), c) rest = 
            pqInsert (cost + c + heuristic p end) (cost + c, s) rest

solve :: String -> Maybe Int
solve input = do
    let grid = parseGrid input
    start <- findChar 'S' grid
    end <- findChar 'E' grid
    astar grid (start, East) end

main :: IO ()
main = do
    input <- getContents
    case solve input of
        Just result -> print result
        Nothing -> putStrLn "No solution found"