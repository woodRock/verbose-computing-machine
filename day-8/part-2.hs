import Text.Read 
import Text.Regex.Posix

type Line = String
type Instruction = (String,Char,Int)
type Accumulator = Int
type Index = Int
type Visited = [Int]

run :: Instruction -> Index -> Accumulator -> (Index, Accumulator)
run ("nop",_,_) i a = (i + 1, a)
run ("acc",'+',x) i a = (i + 1, a + x)
run ("acc",'-',x) i a = (i + 1, a - x)
run ("jmp",'+',x) i a = (i + x, a)
run ("jmp",'-',x) i a = (i - x, a)

execute :: Index -> [Instruction] -> Accumulator -> Visited -> (Bool, Accumulator) 
execute i b a v
    | alreadyVisited = (False , a)
    | terminates = (True , a)
    | otherwise = execute n b da (i:v)
        where
            (n,da) = run (b !! i) i a
            alreadyVisited = i `elem` v
            terminates = i == length b - 1

parse :: Line -> Instruction
parse bc = (op, sign, arg)
    where 
        op = head s
        arg = read $ tail $ s !! 1 
        sign = head $ s !! 1
        s = words bc

possible :: [Line] -> [[Line]]
possible b = map (\u -> map (\(l,i) -> if i == u then update l else l) lines) idx
    where 
        idx = map snd $ filter ((=~ "(nop|jmp)") . fst) lines
        lines = zip b [0..length b - 1] 

update :: Line -> Line 
update l 
    | op == "jmp" = "nop" ++ r
    | otherwise = "jmp" ++ r
    where 
        (op,r) =  splitAt 3 l 

solve :: [Line] -> Maybe Accumulator 
solve bc 
    | null tmp = Nothing 
    | otherwise = Just $ snd $ head tmp
    where 
        tmp = filter fst x
        x = map (\l -> execute 0 l 0 []) trials 
        trials = map (map parse) $ possible bc

main :: IO () 
main = interact $ show . solve . lines
