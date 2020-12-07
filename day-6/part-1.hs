import Data.List
import Data.List.Split

type Answers = String
type Question = String
type Group = [Question]
type Frequency = (Question,Int)

freq :: Answers -> [Frequency]
freq s = map (\x -> ([head x], length x)) . group . sort $ s

solve :: [Group] -> Int 
solve = sum . map (length . freq. concat) 

main :: IO () 
main = interact $ show . solve . map lines . splitOn "\n\n" 
