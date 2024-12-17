import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

data Cell = Empty | Wall | Box | Robot deriving (Eq)
type Position = (Int, Int)
type Grid = [[Cell]]
data Direction = North | South | East | West deriving (Show)

instance Show Cell where
    show Empty = "."
    show Wall = "#"
    show Box = "O"
    show Robot = "@"

parseInput :: String -> (Grid, String)
parseInput input = 
    let ls = lines input
        (gridLines, moveLines) = span (not . null) ls
        grid = map parseLine gridLines
        moves = concat $ drop 1 moveLines
    in (grid, moves)
  where
    parseLine :: String -> [Cell]
    parseLine = map parseCell
    parseCell '#' = Wall
    parseCell 'O' = Box
    parseCell '@' = Robot
    parseCell '.' = Empty
    parseCell _ = Empty

parseMove :: Char -> Maybe Direction
parseMove '^' = Just North
parseMove 'v' = Just South
parseMove '<' = Just West
parseMove '>' = Just East
parseMove _ = Nothing

findRobot :: Grid -> Position
findRobot grid = 
    head [(y,x) | (y, row) <- zip [0..] grid,
                  (x, cell) <- zip [0..] row,
                  cell == Robot]

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

setCell :: Grid -> Position -> Cell -> Grid
setCell grid (y,x) cell =
    take y grid ++
    [take x (grid !! y) ++ [cell] ++ drop (x+1) (grid !! y)] ++
    drop (y+1) grid

canPushBoxes :: Grid -> Direction -> Position -> [Position]
canPushBoxes grid dir startPos = go [] startPos
  where
    go acc curPos =
        let nextPos = movePosition dir curPos
            nextCell = getCell grid nextPos
        in case nextCell of
            Wall -> []
            Empty -> reverse acc
            Box -> go (nextPos : acc) nextPos
            Robot -> []

makeMove :: Grid -> Direction -> Grid
makeMove grid dir =
    let robotPos = findRobot grid
        boxPositions = canPushBoxes grid dir robotPos
    in if null boxPositions 
           then case getCell grid (movePosition dir robotPos) of
                Empty -> moveRobot grid robotPos (movePosition dir robotPos)
                _ -> grid
           else pushBoxes grid dir robotPos boxPositions

pushBoxes :: Grid -> Direction -> Position -> [Position] -> Grid
pushBoxes grid dir robotPos boxPos =
    let grid1 = setCell grid robotPos Empty
        grid2 = foldr (\pos g -> setCell g pos Empty) grid1 boxPos
        newBoxPositions = map (movePosition dir) boxPos
        grid3 = foldr (\pos g -> setCell g pos Box) grid2 newBoxPositions
        newRobotPos = movePosition dir robotPos
    in setCell grid3 newRobotPos Robot

moveRobot :: Grid -> Position -> Position -> Grid
moveRobot grid from to =
    let grid' = setCell grid from Empty
    in setCell grid' to Robot

calculateGPS :: Grid -> Int
calculateGPS grid =
    sum [100 * y + x | 
         (y, row) <- zip [0..] grid,
         (x, cell) <- zip [0..] row,
         cell == Box]

solve :: String -> Int
solve input =
    let (initialGrid, moves) = parseInput input
        finalGrid = foldl makeMove initialGrid (mapMaybe parseMove moves)
    in calculateGPS finalGrid
  where
    mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []

printGrid :: Grid -> String
printGrid = intercalate "\n" . map (concatMap show)

main :: IO ()
main = do
    input <- getContents
    print $ solve input