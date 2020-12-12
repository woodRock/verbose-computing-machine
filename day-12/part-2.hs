import Data.List

type Point = (Double,Double)
type Ship = ((Double, Double), (Double, Double))

manhattan :: Point -> Double
manhattan (x,y) = abs x + abs y

step :: Ship -> String -> Ship
step (spos@(sx, sy), wpos@(wx, wy)) s =
  case s of
    'N' : n -> (spos, (wx, wy + read n))
    'S' : n -> (spos, (wx, wy - read n))
    'E' : n -> (spos, (wx + read n, wy))
    'W' : n -> (spos, (wx - read n, wy))
    'L' : n -> (spos, (wx * cos (a1 n) - wy * sin (a1 n), wx * sin (a1 n) + wy * cos (a1 n)))
    'R' : n -> (spos, (wx * cos (a2 n) - wy * sin (a2 n), wx * sin (a2 n) + wy * cos (a2 n)))
    'F' : n -> ((sx + wx * read n, sy + wy * read n), wpos)
  where
     a1 n = (read n) * 2 * pi / 360
     a2 n = (360 - read n) * 2 * pi / 360

simulate :: [String] -> Ship
simulate = foldl' step ((0, 0), (10, 1))

solve :: [String] -> Int
solve = round . manhattan . fst . simulate

main :: IO ()
main = interact $ show . solve . lines
