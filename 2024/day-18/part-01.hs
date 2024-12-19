import Debug.Trace

type Position = (Int, Int)  -- (col, row) or (x,y)
data Cell = Wall | Empty deriving (Eq)
type Maze = [[Cell]]

instance Show Cell where
    show Wall = "#"
    show Empty = "."

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


-- Walls are passed in as a list of positions, i.e. "1,2"
parse :: String -> Position
parse = (\[a,b] -> (a,b)) . map read . splitOn ","

solve :: [Position] -> Maze 
solve walls = [[if (col, row) `elem` walls then Wall else Empty | col <- [0..6]] | row <- [0..6]]

printMaze :: Maze -> String
printMaze = unlines . map (concatMap show)

main :: IO () 
main = interact $ show . printMaze .  solve . map parse . lines