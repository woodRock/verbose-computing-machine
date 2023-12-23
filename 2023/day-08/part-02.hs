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

allEndWithZ :: [Node] -> Bool 
allEndWithZ a = (==) (length a) . length . filter (\x -> (last x) == 'Z') $ a

solveOne :: Node -> Char -> M.Map Node (Leaf, Leaf) -> Node 
solveOne node i treeMap  
    | i == 'L' = left 
    | otherwise = right
    where 
        (left, right) = fromMaybe ("NULL", "NULL") $ M.lookup node treeMap 

solve:: Count -> [Node] -> [Instruction] -> M.Map Node (Leaf, Leaf) -> Count
solve count nodes (i:is) treeMap
    | allEndWithZ nodes = count 
    | otherwise = solve (count + 1) newNodes is treeMap
    where 
        newNodes = map (\node -> solveOne node i treeMap) nodes

magic:: String -> String
magic x = show $ solve 0 startingNodes instructions treeMap
    where 
        treeMap = M.fromList . parse . drop 2 . lines $ x
        instructions = concat . take 1000000 . repeat . head . lines $ x 
        startingNodes = fst <$> (filter (\(a,b) -> (last a) == 'A') . M.toList $ treeMap)

main :: IO()
main = interact $ magic  