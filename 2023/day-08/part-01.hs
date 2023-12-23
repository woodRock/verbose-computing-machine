import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Node = String 
type Leaf = String 
type Instructions = String

-- Here is an example node.
-- AAA = (BBB, CCC)

parse :: [String] -> [(Node, (Leaf, Leaf))]
parse [] = []
parse (x:xs) = (node, (left, right)) : parse  xs 
    where 
        node = head . words $ x 
        left = init . tail . head . tail . tail . words $ x
        right = init . last . tail . words $ x  

solve:: Int -> Node -> String -> M.Map Node (Leaf, Leaf) -> Int
solve count "ZZZ" _ _ = count
solve count node (i:is) treeMap  
    | i == 'L' = solve (count + 1) left is treeMap 
    | otherwise = solve (count + 1) right is treeMap 
    where 
        (left, right) = fromMaybe ("NULL", "NULL") $ M.lookup node treeMap 

magic:: String -> String 
magic x = show $ solve 0 "AAA" instructions treeMap
    where 
        treeMap = M.fromList . parse . drop 2 . lines $ x
        instructions = concat $ take 1000 $ repeat . head . lines $ x 

main :: IO()
main = interact $ magic  