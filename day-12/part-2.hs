{-# LANGUAGE LambdaCase #-}
import Data.List

type Ship = ((Integer, Integer), (Integer, Integer))

step :: Ship -> String -> Ship
step (spos@(sx, sy), wpos@(wx, wy)) s =
  case s of
    'N' : n -> (spos, (wx, wy + read n))
    'S' : n -> (spos, (wx, wy - read n))
    'E' : n -> (spos, (wx + read n, wy))
    'W' : n -> (spos, (wx - read n, wy))
    'L' : n -> (spos, rotate wpos (360 - read n))
    'R' : n -> (spos, rotate wpos (read n))
    'F' : n ->
      let ni = read n
       in ((sx + wx * ni, sy + wy * ni), wpos)
  where
    rotate (x, y) = \case
      90 -> (y, - x)
      180 -> (- x, - y)
      270 -> (- y, x)

simulate :: [String] -> Ship
simulate =
  foldl' step ((0, 0), (10, 1))

solve :: [String] -> Integer
solve xs =
  let (x, y) = fst $ simulate xs
   in abs x + abs y

main :: IO () 
main = interact $ show . solve . lines
