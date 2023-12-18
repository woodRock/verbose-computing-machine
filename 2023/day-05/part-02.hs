import Data.List.Split (splitOn)
import Data.List (sort, groupBy)

type Map = [[Int]]
type Seed = Int

parseOne:: [String] -> Map
parseOne (_:x) = (map read . words) <$> x

read_seeds :: [Int] -> [Seed]
read_seeds [] = []
read_seeds (x:y:xs) = [x..x+y] ++ read_seeds xs

parse :: String -> ([Seed],[Map])
parse x = (seeds, maps)
    where 
        tmp_seeds = map read . words . head . tail . splitOn "seeds:" . head . lines $ x :: [Int]
        seeds = read_seeds tmp_seeds
        tmp_maps = filter ((/=) [""]) . groupBy (\a b -> a /= "" && b /= "") . tail . splitOn "\n" $ x
        maps = parseOne <$> tmp_maps

solveOne:: Seed -> [Map] -> Int
solveOne seed [] = seed
solveOne seed ([]:xs) = solveOne seed xs
solveOne seed (([m,s,r]:maps):xs)
    | seed >= s && seed < s + r = solveOne ((seed - s) + m) xs
    | otherwise = solveOne seed ((maps):xs)

solve:: ([Seed],[Map]) -> Int
solve (seeds, maps) = minimum $ (\seed -> seed `solveOne` maps) <$> seeds
        
main:: IO() 
main = interact $ show . solve . parse