{-# LANGUAGE ViewPatterns #-}

import Data.List (transpose,sort,group,elem)
import Data.List.Split (splitOn)

canonical :: String -> String 
canonical = min <$> id <*> reverse

monster :: [String]
monster = ["                  # ", 
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

unborder :: [[a]] -> [[a]]
unborder = map (init . tail) . init . tail

isMonster :: [String] -> Bool
isMonster = and . zipWith equal monster
    where 
        equal a b = and $ zipWith compare a b
        compare a b = not (a == '#' && b == '.') 

parse :: String -> (Int,[String])
parse (lines -> (k:v)) = (read . init . last . words $ k, sides v)

sides :: [String] -> [String]
sides x = map canonical $ [head, map last, last, map head] <*> [x] 

solve :: [String] -> Int
solve (map parse -> tiles) = product corners
    where
        unique = concat $ filter ((==1) . length) $ group $ sort $ concatMap snd tiles
        corners = map fst $ filter (\(id,edges) -> (\x -> length x == 2) (filter (== True) (map (`elem` unique) edges))) tiles

main :: IO ()
main = interact $ show . solve . splitOn "\n\n"
