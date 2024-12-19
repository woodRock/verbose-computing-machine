import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type Pattern = B.ByteString
type Design = B.ByteString
type PatternMap = Map.Map Char [Pattern]

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

countPaths :: [Pattern] -> Design -> Int
countPaths patterns design = last dp
  where
    dLen = B.length design
    dp = 1 : [computeDP l | l <- [1..dLen]]
    
    computeDP :: Int -> Int
    computeDP l = sum
        [ dp !! (l - n - 1)
        | n <- [0..l-1]
        , let stub = B.drop (dLen - l) design
        , let prefix = B.take (n + 1) stub
        , prefix `elem` patterns
        ]

solve :: String -> Int
solve input = 
    let (patterns, designs) = parse input
    in sum $ map (countPaths patterns) designs

main :: IO ()
main = do
    input <- getContents
    print $ solve input
