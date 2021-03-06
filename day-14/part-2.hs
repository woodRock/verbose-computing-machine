{-# LANGUAGE TupleSections #-}

import Data.Bits
import qualified Data.Map as M 

type Program = [String]
type BitMask = [(Int,Char)]
type Address = Int
type Value = Int

parse :: BitMask -> Program -> [(BitMask, Address, Value)]
parse _ [] = [] 
parse mask (x:xs)
    | head line == "mask" = parse (zip [35,34..] (last line)) xs
    | otherwise = (mask, r, v) : parse mask xs
    where
        line = words x
        r = read $ init $ drop 4 $ head line 
        v = read $ last line

process :: (BitMask,Address,Value) -> [(Address,Value)]
process (mask,r,v) = map (,v) $ foldr go [r] mask
    where
        go (i,'1') ns = (`setBit` i) <$> ns
        go (i,'0') ns = ns
        go (i,_) ns = [(`clearBit` i), (`setBit` i)] <*> ns

solve :: Program -> Int
solve = sum . M.elems . M.fromList . concatMap process . parse [] 

main :: IO ()
main = interact $ show . solve . lines
