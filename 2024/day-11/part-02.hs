{-# LANGUAGE BangPatterns #-}
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

-- Rules carefully matching the specification
rules :: Int -> [Int]
rules 0 = [1]  -- Rule 1: If the stone is engraved with the number 0
rules n = 
    let s = show n  -- Int already ignores leading zeros
        digitCount = length s
    in if even digitCount
       then  -- Rule 2: Even number of digits
           let mid = digitCount `div` 2
               (l, r) = splitAt mid s
               leftNum = read l :: Int
               rightNum = read r :: Int
           in [leftNum, rightNum]
       else [n * 2024]  -- Rule 3: Multiply by 2024

-- Process one generation using IntMap for efficiency
blinkOnce :: IM.IntMap Int -> IM.IntMap Int
blinkOnce freqMap = 
    IM.foldlWithKey' processStone IM.empty freqMap
    where
        processStone !acc !stone !count =
            let newStones = rules stone
                addStone m s = IM.insertWith (+) s count m
            in foldl' addStone acc newStones

-- Main processing function using IntMap
process :: Int -> [Int] -> Int
process generations initialStones = 
    let initialMap = foldl' (\m n -> IM.insertWith (+) n 1 m) IM.empty initialStones
        finalMap = (!! generations) $ iterate blinkOnce initialMap
    in sum $ IM.elems finalMap

main :: IO ()
main = interact $ show . process 75 . map read . words