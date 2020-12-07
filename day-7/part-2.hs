import Data.List
import Data.List.Split
import Text.Read
import Text.Regex.Posix
import Control.Arrow ((&&&))

type Rule = String
type Bag = String
type Node = (String,Int)

child :: Rule -> Bag 
child = unwords . take 2 . drop 5 . splitOn " "  

count :: Rule -> Int 
count = (\x -> if x == "no" then 1 else read x) . (!! 4) . splitOn " "

node :: Rule -> Node 
node r = (child r, count r)

children :: Rule -> [Node]
children r =  map ((!!) bags &&& (!!) counts) [0..c-1] 
    where
        bags = map unwords $ 
            (\i -> 
                map (\x -> take (snd x + 1 - fst x) $ drop (snd x) i) -- Slice bag with index positions
                [ ((l+1)*4,(l+1)*4+1)  | l <- [0..c-1]] -- Indexes positions for bag colours (start, finish)
            ) s
        counts = map read $ 
            (\i -> 
                map (\x -> i !! x) -- Get count with index position
                [(l + 1)*4 | l <- [0..c-1]] -- Index position for bag counts
            ) s
        s = splitOn " " r
        c = 1 + length (filter (== ',') r)  

tree :: Node -> [Rule] -> Int 
tree (bag,count) r 
    | n == 0 = 0
    | otherwise = count + count * sum (map (`tree` r) cs) 
    where
        cs = children $ head ref
        n = length ref
        ref = filter (=~ bag_regx) r   
        bag_regx = "^" ++ bag 

solve :: [Rule] -> Int
solve = pred . tree ("shiny gold",1) 

main :: IO () 
main = interact $ show . solve . lines

