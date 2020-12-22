import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

parse :: String -> (Int,[String])
parse s = (key,v)
    where 
        key = read $ init $ last $ words k
        (k:v) = lines s

solve :: [String] -> Map Int [String]
solve = M.fromList . map parse

main :: IO ()
main = interact $ show . solve . splitOn "\n\n"
