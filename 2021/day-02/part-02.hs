type Point = [Int]
type Cmd = [String]

move :: Point -> Cmd -> Point
move [x,y,a] [dir,m] 
  | dir == "forward" = [x+d,y+a*d,a] 
  | dir == "up" = [x,y,a-d]
  | dir == "down" = [x,y,a+d]
  where d = read m :: Int

solve :: Point -> [Cmd] -> Int
solve p = product . init . foldl move p

main :: IO () 
main = interact $ show . solve origin . map words . lines
  where 
    origin = [0,0,0]
