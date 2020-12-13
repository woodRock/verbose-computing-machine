import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn) 

parse :: [String] -> (Int,[Int])
parse s = (read $ head s,[read b | b <- splitOn "," $ s !! 1, b /= "x"])

solve :: [String] -> Int
solve s = uncurry (*) $ minimumBy (comparing snd) (map (\b -> (b, b - min `mod` b)) buses)
    where
        (min, buses) = parse s

main :: IO ()
main = interact $ show . solve . lines
