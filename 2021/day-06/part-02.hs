import Data.List.Split

step :: [Int] -> [Int]
step [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]

nSteps n  = (!! n) . iterate step 

count :: Int -> [Int] -> Int
count a = length . filter (== a)

fish :: [Int] -> [Int]
fish x = map (`count` x) [0..8]

main :: IO () 
main = interact $ show . sum . nSteps 256 . fish . parse 
    where 
        parse = \a -> read $ "[" ++ a ++ "]" :: [Int]  

