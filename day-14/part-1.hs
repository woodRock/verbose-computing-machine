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

solve :: Program -> Int
solve = sum . M.elems . M.fromList . map process . parse [] 
    where 
        process (mask,r,v) = (r,foldr go v mask)
        go (i,'1') = (`setBit` i)
        go (i,'0') = (`clearBit` i)
        go (_,_) = id

main :: IO ()
main = interact $ show . solve . lines
