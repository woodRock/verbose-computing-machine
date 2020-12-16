import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Numbers = [Int]
type Turn = Int
type Last = Int

next :: Numbers -> Numbers
next n 
    | first = n ++ [0]
    | otherwise = n ++ [apart] 
    where
        first = l `notElem` init n
        apart = 1 + fromJust (l `elemIndex` reverse (init n))
        l = last n

solve :: Turn -> Numbers -> Last
solve n s = last $ (!! offset) $ iterate next s 
    where offset = n - length s

main :: IO () 
main = interact $ show . solve 2020. map read . splitOn ","
