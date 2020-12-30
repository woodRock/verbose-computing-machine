import qualified Data.Map as M
import Data.Maybe
import Data.List

type DB = M.Map Int Int

parse :: String -> [Int]
parse = map read . words . intersperse ' '

main :: IO ()
main = interact $ show . solve . parse

solve :: [Int] -> Int
solve s = p1*p2
  where
    rawData = s ++ [10..1000000]
    cur = head s
    input = M.fromList $ zip rawData (tail rawData ++ [head rawData])
    tenmillion = snd (iterate move (cur, input) !! 10000000)
    p1 = fromJust $ M.lookup 1 tenmillion
    p2 = fromJust $ M.lookup p1 tenmillion

move :: (Int, DB) ->  (Int, DB)
move (current, db) = (rest, db''') -- c is 3
    where
        a = fromJust $ M.lookup current db -- 8
        b = fromJust $ M.lookup a db -- 9
        c = fromJust $ M.lookup b db -- 1
        rest = fromJust $ M.lookup c db
        db' = M.insert current rest db
        insafter = dest current
        oldValue = fromJust $ M.lookup insafter db'
        db'' = M.insert insafter a db'
        db''' = M.insert c oldValue db''
        dest _cur
            | (_cur-1) < sminimum a b c = smaximum a b c
            | ((_cur-1) /= a) && ((_cur-1) /= b) && ((_cur-1) /= c) = _cur-1
            | otherwise = dest (_cur - 1)

-- from 0
smaximum :: Int -> Int -> Int -> Int
smaximum a b c = maximum ([999997 .. 1000000] \\ [a,b,c])

sminimum :: Int -> Int -> Int -> Int
sminimum a b c = minimum ([1 .. 4] \\ [a,b,c])
