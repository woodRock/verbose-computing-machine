import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Count = Int
type Node = String 
type Leaf = String 
type Instruction = Char

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
solve count [_,_,'Z'] _ _ = count
solve count node (i:is) treeMap  
    | i == 'L' = solve (count + 1) left is treeMap 
    | otherwise = solve (count + 1) right is treeMap 
    where 
        (left, right) = fromMaybe ("NULL", "NULL") $ M.lookup node treeMap 

lcm' :: [Int] -> Int 
lcm' [] = 1
lcm' (x:xs) = lcm x (lcm' xs)

magic:: String -> String
magic x = show $ lcm' $ map (\node -> solve 0 node instructions treeMap) startingNodes 
    where 
        treeMap = M.fromList . parse . drop 2 . lines $ x
        instructions = concat . take 100 . repeat . head . lines $ x 
        startingNodes = fst <$> (filter (\(a,b) -> (last a) == 'A') . M.toList $ treeMap)

main :: IO()
main = interact $ magic  
