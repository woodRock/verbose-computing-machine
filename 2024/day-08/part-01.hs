import Data.Char (isAlphaNum)
import qualified Data.Set as Set

solve :: String -> Int
solve input = length $ valid input $ antinodes $ antennas $ lines input
  where
    antennas grid = [(x, y, c) | (y,row) <- zip [0..] grid, (x,c) <- zip [0..] row, isAlphaNum c]
    
    antinodes pts = concat
        [[(x1 + dx, y1 + dy, '#'), (x2 - dx, y2 - dy, '#')] |
            (x1, y1, c1) <- pts,
            (x2, y2, c2) <- pts,
            c1 == c2, (x1, y1) < (x2, y2), 
            let dx = 2*(x2 - x1), 
            let dy = 2*(y2 - y1)]
    
    valid input = filter inBounds . Set.toList . Set.fromList
      where
        w = length $ head $ lines input
        h = length $ lines input
        inBounds (x,y,_) = x >= 0 && y >= 0 && x < w && y < h

main = interact $ show . solve