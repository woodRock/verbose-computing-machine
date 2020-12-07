import Data.List
import Data.List.Split

type Answers = String
type Group = [Answers]

freq :: Group -> Answers
freq s = filter (\q -> (all (== True) $ map (\p -> q `elem` p) s)) ['a'..'z']

solve :: [Group] -> Int 
solve = sum . map (length . freq) 

main :: IO () 
main = interact $ show . solve . map lines . splitOn "\n\n" 
