import Data.Char (isAlphaNum)
import qualified Data.Set as Set

solve :: String -> Int
solve input = length $ valid input $ antinodes $ antennas $ lines input
  where
    antennas grid = [(x, y, c) | (y,row) <- zip [0..] grid, (x,c) <- zip [0..] row, isAlphaNum c]
    
    antinodes pts = [(x + n*dx, y + n*dy, '#') | 
        (x,y,c1) <- pts, (x2,y2,c2) <- pts, 
        c1 == c2, (x,y) < (x2,y2),
        let dx = x2-x, 
        let dy = y2-y,
        n <- [-50..50]]
    
    valid input = filter inBounds . Set.toList . Set.fromList
      where
        w = length $ head $ lines input
        h = length $ lines input
        inBounds (x,y,_) = x >= 0 && y >= 0 && x < w && y < h

main = interact $ show . solve