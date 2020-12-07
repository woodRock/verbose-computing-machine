import Data.List
import Data.List.Split

type Answers = String
type Question = String
type Group = [Question]
type Frequency = (Question,Int)

freq :: Answers -> [Frequency]
freq s = map (\x -> ([head x], length x)) . group . sort $ s

check :: Group -> Int
check = length . freq . concat 

solve :: [Group] -> Int 
solve = sum . map check 

main :: IO () 
main = interact $ show . solve . map lines . splitOn "\n\n" 
