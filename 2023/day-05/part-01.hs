import Data.List.Split (splitOn)
import Data.List (sort, groupBy)

type Map = [[Int]]
type Seed = Int

parseOne:: [String] -> Map
parseOne (_:x) = (map read . words) <$> x

parse :: String -> ([Seed],[Map])
parse x = (seeds, maps)
    where 
        seeds = map read . words . head . tail . splitOn "seeds:" . head . lines $ x :: [Int]
        tmp = filter ((/=) [""]) . groupBy (\a b -> a /= "" && b /= "") . tail . splitOn "\n" $ x
        maps = parseOne <$> tmp

solveOne:: Seed -> [Map] -> Int
solveOne seed [] = seed
solveOne seed ([]:xs) = solveOne seed xs
solveOne seed (([m,s,r]:maps):xs)
    | seed >= s && seed < s + r = solveOne ((seed - s)+m) xs
    | otherwise = solveOne seed ((maps):xs)

solve:: ([Seed],[Map]) -> Int
solve (seeds, maps) = minimum $ (\seed -> seed `solveOne` maps) <$> seeds
        
main:: IO() 
main = interact $ show . solve . parse