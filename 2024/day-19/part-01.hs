import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Control.Monad (guard)  -- Added this import

type Pattern = B.ByteString
type Design = B.ByteString
type PatternMap = Map.Map Char [Pattern]

data PQEntry = PQEntry {
    position :: Int,
    distance :: Int
} deriving (Eq, Show)

instance Ord PQEntry where
    compare (PQEntry p1 d1) (PQEntry p2 d2) = 
        compare d1 d2 <> compare p1 p2

parse :: String -> ([Pattern], [Design])
parse str = (patterns, designs)
  where
    ls = lines str
    (towels_str, rest) = case ls of
        [] -> ("", [])
        (x:xs) -> (x, xs)
    patterns = map B.pack $ splitOn ',' $ filter (/= ' ') towels_str
    designs = map B.pack $ filter (not . null) rest

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

buildPatternMap :: [Pattern] -> PatternMap
buildPatternMap patterns = Map.fromListWith (++) 
    [(c, [p]) | p <- patterns, 
                not (B.null p), 
                let c = B.head p]

canMakeDesign :: PatternMap -> Design -> Bool
canMakeDesign patternMap design = isJust $ dijkstra patternMap design

dijkstra :: PatternMap -> Design -> Maybe Int
dijkstra patternMap design = go initQueue initVisited
  where
    designLen = B.length design
    initQueue = Set.singleton $ PQEntry 0 0
    initVisited = Set.empty

    go queue visited
        | Set.null queue = Nothing
        | position curr >= designLen = Just $ distance curr
        | Set.member (position curr) visited = go queue' visited
        | otherwise = go queue'' visited'
      where
        curr = Set.findMin queue
        queue' = Set.deleteMin queue
        visited' = Set.insert (position curr) visited
        
        neighbors = do
            let c = B.index design (position curr)
            pattern <- Map.findWithDefault [] c patternMap
            let newPos = position curr + B.length pattern
            guard $ newPos <= designLen
            guard $ pattern `B.isPrefixOf` B.drop (position curr) design
            return $ PQEntry newPos (distance curr + 1)
            
        queue'' = foldr Set.insert queue' 
            [n | n <- neighbors, not $ Set.member (position n) visited]

solve :: String -> Int
solve input = 
    let (patterns, designs) = parse input
        patternMap = buildPatternMap patterns
    in length $ filter (canMakeDesign patternMap) designs

main :: IO ()
main = do
    input <- getContents
    print $ solve input
