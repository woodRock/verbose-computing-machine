import Data.List
import Data.List.Split
import Text.Read
import Text.Regex.Posix

type Rule = String
type Bag = String

parent :: Rule -> Bag 
parent = unwords . take 2 . words  

tree :: Bag -> [Rule] -> [Bag] 
tree b r = b : concatMap (`tree` r) parents    
    where
        parents = map parent $ filter (=~ bag) r :: [Bag]  
        bag = b ++ " (bag|bags)[\\.|,]" :: Bag

solve :: Bag -> [Rule] -> Int
solve b = pred . length . nub . tree b  

main :: IO () 
main = interact $ show . solve "shiny gold" . lines

