import Data.List.Split
import Data.Map (Map, (!))
import qualified Data.Map as M

data Rule = Const Char 
          | Rule [[Int]]
          deriving (Show,Eq)

parse :: String -> Map Int Rule
parse rules = M.fromList $ map f $ lines rules
    where f x = let [k,v] = splitOn ": " x in 
                if head v == '"'
                    then (read k, Const (v !! 1))
                    else (read k, Rule $ map (map read . words) $ splitOn " | " v)

matches :: Map Int Rule -> String -> Bool
matches rules = any null . go (rules M.! 0)
    where go :: Rule -> String -> [String]
          go _ [] = []
          go (Const c) (x:xs) = [xs | c == x]
          go (Rule rs) xs = concatMap (foldl (\ys r -> concatMap (go r) ys) [xs] . map (rules !)) rs

solve :: [String] -> Int
solve [r,m] = length $ filter (matches rules) messages
    where
        rules = parse r
        messages = lines m 

main :: IO ()
main = interact $ show . solve . splitOn "\n\n"
