import Data.List

type Point = (Double,Double)
type Angle = Double
type Ship = (Point,Angle)

manhattan :: Point -> Double
manhattan (x,y) = abs x + abs y

step :: Ship -> String -> Ship
step (spos@(x,y), a) s =
    case s of
        'N' : n -> ((x,y + read n), a)
        'S' : n -> ((x,y - read n), a)
        'E' : n -> ((x - read n,y), a)
        'W' : n -> ((x + read n,y), a)
        'L' : n -> (spos, a - read n)
        'R' : n -> (spos, a + read n)
        'F' : n -> ((x + read n * sin (a * 2 * pi / 360), y + read n * cos (a * 2 * pi / 360)), a)

simulate :: [String] -> Ship
simulate = foldl step ((0,0),0)

solve :: [String] -> Int
solve = round . manhattan . fst . simulate

main :: IO ()
main = interact $ show . solve . lines
