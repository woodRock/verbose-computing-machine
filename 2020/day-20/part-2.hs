{-# LANGUAGE ViewPatterns #-}

import Data.Bool
import Data.Bits
import qualified Data.Map as M
import Data.List.Extra hiding (splitOn)
import Data.List.Split (splitOn)
import Data.Tuple.Extra

data Orientation = Ori Bool Int deriving (Show, Eq, Read, Ord)

t2of3 :: (t1, t2, t) -> t2
t2of3 (_,x,_) = x

showMap :: [[Bool]] -> String
showMap = unlines . map (map (bool '.' '#'))

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)

unborder :: [[[a]]] -> [[[a]]]
unborder = map (tail . init . map (tail . init))

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = foldr1 (.) $ replicate n f

rotgridn :: Int -> [[a]] -> [[a]]
rotgridn n = applyN n rotgrid
    where rotgrid = transpose . reverse

getorients :: [[a]] -> [(Orientation, [[a]])]
getorients g = [(o, orient o g) | o <- orients]
    where
        orients = [Ori flipped nrots | flipped <- [False, True], nrots <- [0..3]]
        orient (Ori False n) = rotgridn n
        orient (Ori True  n) = rotgridn n . reverse

boolsToInt :: [Bool] -> Int
boolsToInt = foldl' (\x y -> x * 2 + bool 0 1 y) 0

hashes :: [String] -> [[Bool]]
hashes = map (map (== '#'))

monsterWidth :: Int
monsterWidth = length . head $ monster

monsterHeight :: Int
monsterHeight = length monster

monster :: [[Bool]]
monster = hashes ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]

monsterSig = mkSig monster

mkSig :: [[Bool]] -> Int
mkSig = boolsToInt . concatMap (take monsterWidth) . take 3 

parse :: [String] -> (Int, [[Bool]])
parse (t:s) = (read $ init t', s')
  where
    (_:t':_) = words t
    s' = hashes s

findMonsters :: [[Bool]] -> Int
findMonsters = maximum . map (uncurry findMonsters' . dupe . snd) . getorients 
  where
    findMonsters' ss0 ss 
        | length ss < monsterHeight = 0
        | otherwise = (if monsterFound then 1 else 0) +
            if length (head ss) > monsterWidth then
                findMonsters' ss0 (map tail ss)
            else
                let ss0' = tail ss0 in findMonsters' ss0' ss0'
        where 
            monsterFound = mkSig ss .&. monsterSig == monsterSig

solve :: [[String]] -> Int
solve (map parse -> tiles) = count True (concat completeGrid) - findMonsters completeGrid * count True (concat monster)
  where
    getInts (tnum, tile) = map (\(o, tile') -> (boolsToInt $ head tile', (o, tnum, tile'))) $ getorients tile
    tileIntMapping = concatMap getInts tiles
    uniqueEdges = filter ((==1) . length) . groupOn fst . sort $ tileIntMapping
    cornerTiles = filter ((==4) . length) . groupOn t2of3 . sortOn t2of3 . map (snd . head)  $ uniqueEdges
    tileMap = M.fromListWith (++) $ map (\(edge, (_, tnum, tile)) -> (edge, [(tnum, tile)])) tileIntMapping
    stTiles = filter (\(Ori fl _, _, _) -> not fl) $ head cornerTiles
    stGrid = let [(o1, t1), (o2, t2)] = map (\(Ori _ n, _, t) -> (n, t)) stTiles
             in  if o2 == succ o1 `mod` 4 then t1 else t2
    stTile = (t2of3 $ head stTiles, stGrid)
    belowTiles (n, t) = case filter ((/=n) . fst) $ tileMap M.! boolsToInt (last t) of
                          [nexttile] -> (n, t):belowTiles nexttile
                          _          -> [(n, t)]
    rightTiles (n, t) = map (transpose . snd) $ belowTiles (n, transpose t)
    allTiles = map (unborder . rightTiles) $ belowTiles stTile
    allTilesRows = map (foldl1' (zipWith (++))) allTiles
    completeGrid = concat allTilesRows

main :: IO ()
main = interact $ show . solve . map lines . splitOn "\n\n"
