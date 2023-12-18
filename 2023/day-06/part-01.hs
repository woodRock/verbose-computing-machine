import Data.List.Split (splitOn)

type Distance = Float
type Time = Float

parse :: String -> ([Time],[Distance])
parse x = (times, distances)
    where
        [tmp_time,tmp_distance] = lines x
        times = map read . words . head . tail . splitOn "Time:" $ tmp_time :: [Time]
        distances = map read . words . head . tail . splitOn "Distance:" $ tmp_distance :: [Distance]

solve :: ([Time],[Distance]) -> Int
solve (times, distances) = product $ results
    where 
        races = zip times distances
        race (t_max, s_best) = length $ filter ((<) s_best) $ map (\t -> t * (t_max - t)) [0..t_max]
        results = race <$> races 

main :: IO()
main = interact $ show . solve . parse
