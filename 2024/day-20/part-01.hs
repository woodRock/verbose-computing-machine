import Data.List (nub, minimumBy)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Array.Unboxed
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Data.Ord (comparing)
import Control.Monad (guard)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..))

type Position = (Int, Int)
type Grid = UArray Position Char
type Cost = Int
type DistanceCache = Map.Map Position Cost
type State = (Position, Maybe Position, Int, Cost)  -- (location, cheat start position, count, distance)
type Seen = Set.Set (Position, Maybe Position)
type SavedCosts = Map.Map (Position, Maybe Position) Cost

moves :: [Position]
moves = [(0,1), (0,-1), (1,0), (-1,0)]

parseGrid :: String -> (Grid, Position, Position)
parseGrid input = 
    let rows = lines input
        nRows = length rows
        nCols = length (head rows)
        bounds = ((0,0), (nRows-1, nCols-1))
        assocs = [((i,j), c) | (i, row) <- zip [0..] rows
                            , (j, c) <- zip [0..] row]
        start = head [pos | (pos, 'S') <- assocs]
        end = head [pos | (pos, 'E') <- assocs]
    in (array bounds assocs, start, end)

addPos :: Position -> Position -> Position
addPos (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

inBounds :: Grid -> Position -> Bool
inBounds grid pos = inRange (bounds grid) pos

buildDistanceCache :: Grid -> Position -> DistanceCache
buildDistanceCache grid start = go (Seq.singleton (0, start)) Map.empty
  where
    go queue cache = case Seq.viewl queue of
        EmptyL -> cache
        (cost, pos) :< rest
            | Map.member pos cache -> go rest cache
            | otherwise -> 
                let cache' = Map.insert pos cost cache
                    nextMoves = [(cost + 1, newPos) | 
                                move <- moves,
                                let newPos = addPos pos move,
                                inBounds grid newPos,
                                grid ! newPos /= '#',
                                not (Map.member newPos cache)]
                    newQueue = foldl (|>) rest nextMoves
                in go newQueue cache'

cheatBFS :: Grid -> DistanceCache -> Position -> Int -> Int -> Int
cheatBFS grid costs start maxCount target = 
    length $ filter (\(_, cost) -> costs Map.! start - cost >= target) $
    Map.toList $ go initialQueue Set.empty Map.empty
  where
    initialQueue = Seq.singleton (start, Nothing, maxCount, 0)
    
    go :: Seq State -> Seen -> SavedCosts -> SavedCosts
    go queue seen saved = case Seq.viewl queue of
        EmptyL -> saved
        (loc, cheatStart, count, dist) :< rest
            | count < 0 || Set.member (loc, cheatStart) seen -> go rest seen saved
            | isJust cheatStart -> 
                let saved' = if grid ! loc /= '#' && 
                              costs Map.! loc + dist < costs Map.! start
                           then Map.insert (loc, cheatStart) (costs Map.! loc + dist) saved
                           else saved
                    next = [(newPos, cheatStart, count-1, dist+1) |
                           move <- moves,
                           let newPos = addPos loc move,
                           inBounds grid newPos]
                    newQueue = foldl (|>) rest next
                in go newQueue (Set.insert (loc, cheatStart) seen) saved'
            | otherwise ->
                let startCheatingMoves = [(newPos, Just loc, count-1, dist+1) |
                                        move <- moves,
                                        let newPos = addPos loc move,
                                        inBounds grid newPos]
                    normalMoves = [(newPos, Nothing, count, dist+1) |
                                 move <- moves,
                                 let newPos = addPos loc move,
                                 inBounds grid newPos,
                                 grid ! newPos /= '#']
                    newQueue = foldl (|>) rest (startCheatingMoves ++ normalMoves)
                in go newQueue (Set.insert (loc, cheatStart) seen) saved

solve :: String -> (Int, Int)
solve input = 
    let (grid, start, end) = parseGrid input
        costs = buildDistanceCache grid end
        part1 = cheatBFS grid costs start 2 100
        part2 = cheatBFS grid costs start 20 100
    in (part1, part2)

main :: IO ()
main = do
    input <- getContents
    let (part1, part2) = solve input
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2