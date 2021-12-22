import Text.Read 
import Text.Regex.Posix
import Data.Maybe

type Line = String
type Instruction = (String,Char,Int)
type Accumulator = Int
type Index = Int
type Visited = [Int]

update :: Line -> Line 
update l 
    | op == "jmp" = "nop" ++ r
    | otherwise = "jmp" ++ r
    where 
        (op,r) =  splitAt 3 l 

possible :: [Line] -> [[Line]]
possible b = map (\u -> map (uncurry (updateIfIndex u)) lines) idx
    where 
        idx = map snd $ filter ((=~ "(nop|jmp)") . fst) lines
        lines = zip b [0..length b - 1] 
        updateIfIndex u l i 
            | i == u = update l 
            | otherwise = l

parse :: Line -> Instruction
parse bc = (op, sign, arg)
    where 
        op = head s
        arg = read $ tail $ s !! 1 
        sign = head $ s !! 1
        s = words bc

run :: Instruction -> Index -> Accumulator -> (Index, Accumulator)
run ("nop",_,_) i a = (i + 1, a)
run ("acc",'+',x) i a = (i + 1, a + x)
run ("acc",'-',x) i a = (i + 1, a - x)
run ("jmp",'+',x) i a = (i + x, a)
run ("jmp",'-',x) i a = (i - x, a)

execute :: Index -> [Instruction] -> Accumulator -> Visited -> Maybe Accumulator 
execute i b a v
    | infiniteLoop = Nothing
    | terminates = Just a
    | otherwise = execute n b da (i:v)
        where
            (n,da) = run (b !! i) i a
            infiniteLoop = i `elem` v
            terminates = i == length b - 1

solve :: [Line] -> Maybe Accumulator 
solve bc 
    | null tmp = Nothing 
    | otherwise = head tmp
    where 
        tmp = filter isJust x
        x = map (\l -> execute 0 l 0 []) trials 
        trials = map (map parse) $ possible bc

main :: IO () 
main = interact $ show . solve . lines
