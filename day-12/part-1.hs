{-# LANGUAGE LambdaCase #-}

import Data.List

type Point = (Int,Int)
type Direction = (Int,Int)
type Ship = (Point,Direction)

manhattan :: Point -> Int
manhattan (x,y) = abs x + abs y

toDirection :: Char -> Direction
toDirection 'N' = (0,1)
toDirection 'E' = (1,0)
toDirection 'S' = (0,-1)
toDirection 'W' = (-1,0)

direction :: Direction -> Char
direction (0,1)  = 'N'
direction (1,0)  = 'E'
direction (0,-1) = 'S'
direction (-1,0) = 'W'

step :: Ship -> String -> Ship
step (spos@(sx,sy), wpos@(wx,wy)) s =
    case s of
        'N' : n -> ((sx,sy + read n), wpos)
        'S' : n -> ((sx,sy - read n), wpos)
        'E' : n -> ((sx + read n,sy), wpos)
        'W' : n -> ((sx - read n,sy), wpos)
        'L' : n -> (spos, rotate wpos (360 - read n))
        'R' : n -> (spos, rotate wpos (read n))
        'F' : n ->
            let ni = read n
            in ((sx + wx * ni, sy + wy * ni), wpos)
        where
            rotate (x,y) = \case
                90 -> (y,-x)
                180 -> (-x,-y)
                270 -> (-y,x)

simulate :: [String] -> Ship
simulate = foldl step (origin,facing)
    where
        facing = toDirection 'E'
        origin = (0,0)

solve :: [String] -> Int
solve xs = manhattan s
    where
        (s,d) = simulate xs
        origin = (0,0)

main :: IO ()
main = interact $ show . solve . lines
