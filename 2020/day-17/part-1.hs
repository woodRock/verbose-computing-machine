import qualified Data.Set as S
import Data.List (elemIndices)

data Position = P2 Int Int | P3 Int Int Int  | P4 Int Int Int Int deriving (Show,Ord,Eq)
type Space = S.Set Position -- same type used for both active space and surrounding space

main = do
  space2D <- read2D <$> readFile "input.txt"
  print $ solve 6 . S.map makeP3 $ space2D -- Part 1
  print $ solve 6 . S.map makeP4 $ space2D -- Part 2

solve :: Int -> Space -> Int
solve turns start = S.size (iterate evolve start !! turns)

evolve :: Space -> Space
evolve s = S.filter (willLive s) (allAdjPos s)

willLive :: Space -> Position -> Bool
willLive s p = (neighbors == 3) || (neighbors == 2 && isAlive) 
  where isAlive   = S.member p s
        neighbors = S.size $ S.intersection (adjPos p) s

allAdjPos :: Space -> Space  -- excludes active cells with no neighbors. OK in this game, becasue they die anyway 
allAdjPos = S.foldr (S.union . adjPos) S.empty     

adjPos :: Position -> Space -- the set of all positions adjacent to position
adjPos p@(P3 x y z)   = S.fromList [p' | x1<-ad x, y1<-ad y, z1<-ad z,           let p' = P3 x1 y1 z1,    p' /= p]
adjPos p@(P4 x y z w) = S.fromList [p' | x1<-ad x, y1<-ad y, z1<-ad z, w1<-ad w, let p' = P4 x1 y1 z1 w1, p' /= p]

ad n = [(n-1)..(n+1)]

read2D :: String -> Space -- (P2 Space)
read2D f = S.fromList [P2 x y | (y,row) <- zip [0..] (lines f), x <- elemIndices '#' row]

makeP3 (P2 x y) = P3 x y 0
makeP4 (P2 x y) = P4 x y 0 0
