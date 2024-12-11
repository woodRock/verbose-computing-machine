import qualified Data.IntMap.Strict as IM

rules :: Int -> [Int]
rules 0 = [1]
rules n 
    | even len = [read l, read r]
    | otherwise = [n * 2024]
    where s = show n
          len = length s
          (l, r) = splitAt (len `div` 2) s

blink :: IM.IntMap Int -> IM.IntMap Int
blink = IM.foldlWithKey' (\m k v -> foldr (flip (IM.insertWith (+)) v) m (rules k)) IM.empty

main :: IO ()
main = interact $ show . sum . IM.elems . (!! 75) . iterate blink . 
       foldr (\k -> IM.insertWith (+) k 1) IM.empty . map read . words