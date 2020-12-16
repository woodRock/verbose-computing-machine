import Data.List
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M

type Numbers = [Int]
type Turn = Int
type Last = Int
type Index = Int
type Moves = M.IntMap Int

next :: (Turn, Moves, Last) -> (Turn, Moves, Last)
next (turn,moves,last) = (turn + 1,M.insert last turn moves,out)
    where
        out = case M.lookup last moves of 
            Nothing -> 0
            Just x -> turn - x

solve :: Turn -> Numbers -> Last
solve n = (\(_,_,x) -> x) . until (\(i,_,_) -> i == n) next .
        (\l -> (length l, M.fromList (zip (init l) [1..]),last l))

main :: IO () 
main = interact $ show . solve (3*10^7) . map read . splitOn ","
