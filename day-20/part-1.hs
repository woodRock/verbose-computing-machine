{-# LANGUAGE ViewPatterns #-}

import Data.List (transpose,sort,group,elem)
import Data.List.Split (splitOn)

type Id = Int
type Edge = String

canonical :: Edge -> Edge
canonical = min <$> id <*> reverse

parse :: String -> (Id,[Edge])
parse (lines -> (k:v)) =  (key,value)
    where 
        key = read $ init $ last $ words k
        value = map (canonical . head) $ take 4 $ iterate (transpose . reverse) v

solve :: [String] -> Int
solve (map parse -> tiles) = product corners
    where
        corners = map fst $ filter (\(id,edges) -> (\x -> length x == 2) (filter (== True) (map (`elem` unique) edges))) tiles
        unique = concat $ filter ((==1) . length) $ group $ sort $ concatMap snd tiles

main :: IO ()
main = interact $ show . solve . splitOn "\n\n"
