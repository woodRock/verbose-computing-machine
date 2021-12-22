import Data.List
import Data.List.Split
import Text.Read
import Text.Regex.Posix

type Rule = String
type Bag = String
type Node = (Bag,Int)

slice :: (Int, Int) -> [String] -> [String]
slice (x,y) = take (y + 1 - x) . drop y 

branch :: Rule -> [Node]
branch r =  zip bags counts 
    where
        bags = map (unwords . (`slice` s)) [((l+1)*4,(l+1)*4+1)  | l <- [0..c]]
        counts = map (read . (s !!)) [(l + 1)*4 | l <- [0..c]] 
        s = words r
        c = length (filter (== ',') r)  

tree :: Node -> [Rule] -> Int 
tree (bag,count) r 
    | leaf = 0
    | otherwise = count * (1 + sum (map (`tree` r) branches)) 
    where
        leaf = null references 
        references = filter (=~ ("^" ++ bag)) r   
        branches = branch $ head references

solve :: [Rule] -> Int
solve = pred . tree ("shiny gold",1)

main :: IO () 
main = interact $ show . solve . lines

