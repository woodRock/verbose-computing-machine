import Data.List (find, nub)
import qualified Data.Map as Map

data Direction = North | East | South | West deriving (Eq, Show, Ord)
type Position = (Int, Int)  -- (row, col)
type State = (Position, Direction)
type Grid = [(Position, Char)]
type Cost = Int
type Path = [Position]

parseGrid :: String -> [(Position, Char)]
parseGrid input = [((i, j), c)
                 | (i, row) <- zip [0..] (lines input)
                 , (j, c) <- zip [0..] row]

findChar :: Char -> Grid -> Maybe Position
findChar target = fmap fst . find ((== target) . snd)

move :: Direction -> Position -> Position
move North (r, c) = (r-1, c)
move South (r, c) = (r+1, c)
move East  (r, c) = (r, c+1)
move West  (r, c) = (r, c-1)

gridLookup :: Position -> Grid -> Maybe Char
gridLookup pos = fmap snd . find ((== pos) . fst)

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

-- Modified to track minimum cost to reach each state
findOptimalPaths :: Grid -> State -> Position -> ([Path], Cost)
findOptimalPaths grid start@(startPos, _) end = 
    search (Map.singleton start 0) [(0, start, [startPos])] [] Nothing
  where
    search _ [] paths Nothing = ([], maxBound)  -- No solution found
    search _ [] paths (Just minCost) = (paths, minCost)
    search costs ((cost, state@(pos, dir), path):rest) paths minCost
        | pos == end = case minCost of
            Nothing -> search costs rest [reverse path] (Just cost)
            Just mc 
                | cost < mc -> search costs rest [reverse path] (Just cost)
                | cost == mc -> search costs rest (reverse path : paths) minCost
                | otherwise -> search costs rest paths minCost
        | maybe False (cost >) minCost = search costs rest paths minCost
        | otherwise = 
            let nexts = [(cost + c, s, fst s : path) 
                       | (s, c) <- nextStates grid state,
                         costOk costs s (cost + c)]
                newCosts = foldr (\(c, s, _) m -> Map.insert s c costs) 
                                costs nexts
            in search newCosts (rest ++ nexts) paths minCost
      where
        costOk costs state newCost = case Map.lookup state costs of
            Nothing -> True
            Just oldCost -> newCost <= oldCost

showGrid :: Grid -> [Position] -> String
showGrid grid marked = unlines rows
  where
    maxRow = maximum [r | ((r,_),_) <- grid]
    maxCol = maximum [c | ((_,c),_) <- grid]
    rows = [[ getChar (i,j) | j <- [0..maxCol]] | i <- [0..maxRow]]
    getChar pos = case (gridLookup pos grid, pos `elem` marked) of
        (Just c, True) -> 'O'  -- Mark optimal path positions
        (Just c, False) -> c   -- Original character
        _ -> '#'               -- Wall or out of bounds

main :: IO ()
main = do
    input <- getContents
    let grid = parseGrid input
        origGrid = [(p,c) | (p,c) <- grid, c /= 'O']  -- Remove any existing O marks
    case (findChar 'S' origGrid, findChar 'E' origGrid) of
        (Just start, Just end) -> do
            let (optimalPaths, minCost) = findOptimalPaths origGrid (start, East) end
                optimalPositions = nub $ concat optimalPaths
                pathCount = length optimalPaths
            putStrLn $ "Minimum cost: " ++ show minCost
            putStrLn $ "Number of optimal paths found: " ++ show pathCount
            putStrLn $ "Number of unique positions in optimal paths: " ++ show (length optimalPositions)
            putStrLn "Visualization of optimal seats (marked with O):"
            putStrLn $ showGrid origGrid optimalPositions
        _ -> putStrLn "Invalid grid: no start or end found"