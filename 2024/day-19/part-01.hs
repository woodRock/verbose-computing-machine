import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B
import Control.DeepSeq (force)

type Pattern = B.ByteString
type Design = B.ByteString
type Cache = Map.Map B.ByteString Bool
type PatternMap = Map.Map Char [Pattern]  -- Changed back to Map for safety

parse :: String -> ([Pattern], [Design])
parse str = (patterns, designs)
  where
    (towels_str, _:designs_str) = span (not . null) (lines str)
    patterns = map B.pack $ splitOn ',' $ filter (/= ' ') $ head towels_str
    designs = map B.pack $ filter (not . null) designs_str

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

canMakeDesign :: PatternMap -> Pattern -> Bool
canMakeDesign patternMap design = go design Map.empty
  where
    go curr cache
        | B.null curr = True
        | otherwise = case Map.lookup curr cache of
            Just result -> result
            Nothing -> 
                let c = B.head curr
                    possible = Map.findWithDefault [] c patternMap
                    validPatterns = filter (`B.isPrefixOf` curr) possible
                    result = any continueWithPattern validPatterns
                in result `seq` Map.insert curr result cache `seq` result
      where
        continueWithPattern p = 
            let remaining = B.drop (B.length p) curr
            in B.length remaining < B.length curr && 
               go remaining (Map.insert curr False cache)

solve :: String -> Int
solve input = 
    let (patterns, designs) = parse input
        patternMap = buildPatternMap patterns
    in length $ filter (canMakeDesign patternMap) designs

main :: IO ()
main = do
    input <- getContents
    print $ solve input