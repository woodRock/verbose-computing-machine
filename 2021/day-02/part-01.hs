type Point = [Int]
type Cmd = [String]

move :: Point -> Cmd -> Point
move [x,y] [dir,a] 
  | dir == "forward" = [x+d,y] 
  | dir == "up" = [x,y-d]
  | dir == "down" = [x,y+d]
  where d = read a :: Int

solve :: Point -> [Cmd] -> Int
solve p = product . foldl move p

main :: IO () 
main = interact $ show . solve origin . map words . lines
  where 
    origin = [0,0]
