import Data.List.Split (splitOn)
import Data.List (group, sort, concat)

type Point = (Int,Int)
type Line = (Point,Point)

count :: [[Point]] -> Int 
count = length . filter (\xs -> length xs > 1) . group . sort . concat

solve :: [Line] -> Int
solve = count . map foo . filter isHV

isHV :: Line -> Bool
isHV ((x,y),(x',y')) = x == x' || y == y'

foo :: Line -> [Point]
foo ((x1,y1),(x2,y2)) = [(x1 + n*dx, y1+n*dy) | n <- [0..(max (abs (x2-x1)) (abs (y2-y1)))]]
    where 
        dx = comp' x1 x2
        dy = comp' y1 y2

comp' :: Ord a => a -> a -> Int
comp' a b | b > a = 1
          | b < a = -1 
          | otherwise = 0

parse :: String -> Line 
parse = pack . map (pack . map stoi . splitOn ",") . splitOn "->"
    where pack (x:y:_) = (x,y)
          stoi s = read s :: Int 

main :: IO () 
main = interact $ show . solve . map parse . lines 
