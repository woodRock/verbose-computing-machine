{-# LANGUAGE BangPatterns #-}
import qualified Data.Set as Set

type Point = (Int, Int)
type Grid = [[Char]]

neighbors :: Point -> [Point]
neighbors (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

findRegion :: Grid -> Point -> Set.Set Point
findRegion grid start = go Set.empty [start]
  where
    width = length (head grid)
    height = length grid
    symbol = grid !! snd start !! fst start
    
    inBounds (x,y) = x >= 0 && y >= 0 && x < width && y < height
    
    go !visited [] = visited
    go !visited (p:ps) 
      | Set.member p visited = go visited ps
      | not (inBounds p) = go visited ps
      | grid !! snd p !! fst p /= symbol = go visited ps
      | otherwise = 
          let !newVisited = Set.insert p visited
              validNeighbors = filter inBounds $ neighbors p
              newPoints = filter (\n -> grid !! snd n !! fst n == symbol) validNeighbors
          in go newVisited (ps ++ newPoints)

countSides :: Grid -> Set.Set Point -> Int
countSides grid region = horizontalSides + verticalSides
  where
    width = length (head grid)
    height = length grid
    isInRegion = flip Set.member region

    horizontalSides = go 0 0
      where
        go !sum !y | y > height = sum
        go !sum !y = go (sum + countRow y) (y + 1)
        
        countRow y = countRun 0 False 0
          where
            countRun !count _ !x | x >= width = count
            countRun !count !currentSide !x =
              let topCell = y > 0 && isInRegion (x,y-1)
                  bottomCell = y < height && isInRegion (x,y)
                  !newCount = if topCell /= bottomCell && not currentSide
                             then count + 1
                             else count
                  !newSide = topCell /= bottomCell
              in countRun newCount newSide (x+1)

    verticalSides = go 0 0
      where
        go !sum !x | x > width = sum
        go !sum !x = go (sum + countCol x) (x + 1)
        
        countCol x = countRun 0 False 0
          where
            countRun !count _ !y | y >= height = count
            countRun !count !currentSide !y =
              let leftCell = x > 0 && isInRegion (x-1,y)
                  rightCell = x < width && isInRegion (x,y)
                  !newCount = if leftCell /= rightCell && not currentSide
                             then count + 1
                             else count
                  !newSide = leftCell /= rightCell
              in countRun newCount newSide (y+1)

solve :: Grid -> Int
solve grid = go Set.empty [] allPoints
  where
    width = length (head grid)
    height = length grid
    allPoints = [(x,y) | y <- [0..height-1], x <- [0..width-1]]

    go !visited !regions [] = sum $ map calculatePrice regions
    go !visited !regions (p:ps)
      | Set.member p visited = go visited regions ps
      | otherwise = 
          let !region = findRegion grid p
              !newVisited = Set.union visited region
          in go newVisited (region:regions) ps
    
    calculatePrice r = Set.size r * countSides grid r

main :: IO ()
main = do
  input <- getContents
  print . solve . lines $ input

test :: IO ()
test = do
  putStrLn "Testing example 1 (should be 80):"
  print $ solve ["AAAA", "BBCD", "BBCC", "EEEC"]
  putStrLn "Testing example 2 (should be 436):"
  print $ solve ["OOOOO", "OXOXO", "OOOOO", "OXOXO", "OOOOO"]