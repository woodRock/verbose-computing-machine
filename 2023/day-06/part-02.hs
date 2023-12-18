import Data.List.Split (splitOn)

type Distance = Float
type Time = Float

parse :: String -> (Time,Distance)
parse x = (times, distances)
    where
        [tmp_time,tmp_distance] = lines x
        times = read . filter ((/=) ' ') . head . tail . splitOn "Time:" $ tmp_time :: Time 
        distances = read . filter ((/=) ' ') . head . tail . splitOn "Distance:" $ tmp_distance :: Distance

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

solve :: (Time,Distance) -> Int
solve (time, distance) =  winners
    where 
        races = (time, distance)
        a = -1 -- Acceleration
        h = time / 2 -- Vertex x 
        k = h ^ 2 -- Vertex y  
        y x = a * (x - h) ^ 2 + k -- Parabola
        x_1 y = - sqrt ((y - k) / a) + h -- Solve for x 
        lower_bound = x_1 distance
        peak_is_int = isInt $ y h
        peak_round = if peak_is_int then 1 else 0
        -- root_is_int = isInt $ lower_bound
        -- root_round = if root_is_int then 0 else 2
        rounding_error = peak_round  -- + root_round
        winners = round(2 * (h - lower_bound)) - rounding_error

main :: IO()
main = interact $ show . solve . parse
