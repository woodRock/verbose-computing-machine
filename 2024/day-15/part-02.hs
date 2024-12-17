module Day15 where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Debug.Trace
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Data.Sequence.Internal (ViewL(..))
import qualified Data.Foldable as F

data Cell = Empty | Wall | BoxLeft | BoxRight | Robot deriving (Eq)
type Position = (Int, Int)
type Grid = [[Cell]]
data Direction = North | South | East | West deriving (Show, Eq)

-- A move consists of old position, new position, and the cell to move
type Move = (Position, Position, Cell)

instance Show Cell where
    show Empty = "."
    show Wall = "#"
    show BoxLeft = "["
    show BoxRight = "]"
    show Robot = "@"

parseInput :: String -> (Grid, String)
parseInput input = 
    let ls = lines input
        (gridLines, moveLines) = span (not . null) ls
        grid = expandGrid $ map parseLine gridLines
        moves = if null moveLines then "" else concat $ drop 1 moveLines
    in trace ("\nInitial grid:\n" ++ printGrid grid) (grid, moves)
  where
    parseLine = map parseCell
    parseCell '#' = Wall
    parseCell 'O' = BoxLeft
    parseCell '@' = Robot
    parseCell '.' = Empty
    parseCell _ = Empty

expandGrid :: [[Cell]] -> [[Cell]]
expandGrid = map expandRow
  where
    expandRow = concatMap expandCell
    expandCell Wall = [Wall, Wall]
    expandCell BoxLeft = [BoxLeft, BoxRight]
    expandCell Robot = [Robot, Empty]
    expandCell Empty = [Empty, Empty]

movePosition :: Direction -> Position -> Position
movePosition North (y,x) = (y-1,x)
movePosition South (y,x) = (y+1,x)
movePosition West  (y,x) = (y,x-1)
movePosition East  (y,x) = (y,x+1)

getCell :: Grid -> Position -> Cell
getCell grid (y,x)
    | y < 0 || y >= length grid = Wall
    | x < 0 || x >= length (head grid) = Wall
    | otherwise = grid !! y !! x

findRobot :: Grid -> Position
findRobot grid = 
    head [(y,x) | (y, row) <- zip [0..] grid,
                  (x, cell) <- zip [0..] row,
                  cell == Robot]

setCell :: Grid -> Position -> Cell -> Grid
setCell grid (y,x) cell =
    take y grid ++
    [take x (grid !! y) ++ [cell] ++ drop (x+1) (grid !! y)] ++
    drop (y+1) grid

makeMove :: Grid -> Direction -> Grid
makeMove grid dir =
    let robotPos = findRobot grid
        nextPos = movePosition dir robotPos
        initialMoves = [(robotPos, nextPos, Robot)]
        (finalMoves, success) = processMovements grid dir (Seq.fromList [nextPos]) (Seq.fromList initialMoves)
    in if success
       then applyMoves grid (F.toList finalMoves)
       else grid

processMovements :: Grid -> Direction -> Seq Position -> Seq Move -> (Seq Move, Bool)
processMovements grid dir pendingPos moves =
    case Seq.viewl pendingPos of
        EmptyL -> (moves, True)
        pos :< restPending -> 
            case getCell grid pos of
                Robot -> (moves, False)  -- Found second robot
                Wall -> (moves, False)   -- Hit a wall
                Empty -> processMovements grid dir restPending moves
                BoxLeft -> case dir of
                    West -> (moves, False)  -- Invalid: pushing left bracket leftward
                    East -> do
                        let pos2 = movePosition dir pos
                            pos3 = movePosition dir pos2
                        if getCell grid pos2 /= BoxRight then (moves, False)
                        else processMovements grid dir 
                                (restPending |> pos3)
                                (moves |> (pos, pos2, BoxLeft) |> (pos2, pos3, BoxRight))
                    _ -> do  -- Vertical movement
                        let pos2 = movePosition East pos
                        if getCell grid pos2 /= BoxRight then (moves, False)
                        else let nextPos1 = movePosition dir pos
                                 nextPos2 = movePosition dir pos2
                             in processMovements grid dir 
                                    (restPending |> nextPos1 |> nextPos2)
                                    (moves |> (pos, nextPos1, BoxLeft) |> (pos2, nextPos2, BoxRight))
                BoxRight -> case dir of
                    East -> (moves, False)  -- Invalid: pushing right bracket rightward
                    West -> do
                        let pos2 = movePosition dir pos
                            pos3 = movePosition dir pos2
                        if getCell grid pos2 /= BoxLeft then (moves, False)
                        else processMovements grid dir 
                                (restPending |> pos3)
                                (moves |> (pos2, pos3, BoxLeft) |> (pos, pos2, BoxRight))
                    _ -> do  -- Vertical movement
                        let pos2 = movePosition West pos
                        if getCell grid pos2 /= BoxLeft then (moves, False)
                        else let nextPos1 = movePosition dir pos2
                                 nextPos2 = movePosition dir pos
                             in processMovements grid dir 
                                    (restPending |> nextPos1 |> nextPos2)
                                    (moves |> (pos2, nextPos1, BoxLeft) |> (pos, nextPos2, BoxRight))

applyMoves :: Grid -> [Move] -> Grid
applyMoves grid moves = 
    let clearOld = foldr (\(old, _, item) g -> 
                            if getCell g old == item
                            then setCell g old Empty
                            else g) grid moves
    in foldr (\(_, new, item) g -> setCell g new item) clearOld moves

calculateGPS :: Grid -> Int
calculateGPS grid =
    sum [100 * y + x | 
         (y, row) <- zip [0..] grid,
         (x, cell) <- zip [0..] row,
         cell == BoxLeft]

printGrid :: Grid -> String
printGrid = intercalate "\n" . map (concatMap show)

parseMove :: Char -> Maybe Direction
parseMove '^' = Just North
parseMove 'v' = Just South
parseMove '<' = Just West
parseMove '>' = Just East
parseMove _ = Nothing

solve :: String -> Int
solve input =
    let (initialGrid, moves) = parseInput input
        finalGrid = foldl makeMove initialGrid (mapMaybe parseMove moves)
    in trace ("\nFinal state:\n" ++ printGrid finalGrid) $
       calculateGPS finalGrid
  where
    mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []

main :: IO ()
main = interact $ show . solve