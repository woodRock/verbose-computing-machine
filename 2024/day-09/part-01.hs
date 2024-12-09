import Data.Char (isAlphaNum)
import qualified Data.Set as Set

type Point = (Int, Int, Char)

findAntenna :: [String] -> [Point]
findAntenna grid = [(x, y, c) | 
    (y, row) <- zip [0..] grid, 
    (x, c) <- zip [0..] row, 
    isAlphaNum c]

findAntinodes :: [Point] -> [Point]
findAntinodes antennas = concat
    [[(x1 + dx, y1 + dy, '#'), (x2 - dx, y2 - dy, '#')] |
        (x1, y1, c1) <- antennas,
        (x2, y2, c2) <- antennas,
        c1 == c2,            -- Check where the antennas are the same.
        (x1, y1) < (x2, y2), -- Ignore self comparison.
        let dx = 2*(x2 - x1),
        let dy = 2*(y2 - y1)]

solve :: [String] -> Int
solve grid = length $ filter (isInBounds bounds) $ Set.toList $ Set.fromList antinodes
    where
        bounds = (length (head grid), length grid)
        antinodes = findAntinodes $ findAntenna grid
        isInBounds (w, h) (x, y, _) = x >= 0 && y >= 0 && x < w && y < h

main :: IO ()
main = interact $ show . solve . lines