{-# LANGUAGE ViewPatterns #-}

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

execute :: Index -> [Instruction] -> Accumulator -> Visited -> Accumulator
execute i b a v
    | alreadyVisited = a
    | otherwise = execute n b da (i:v)
        where
            (n,da) = run (b !! i) i a
            alreadyVisited = i `elem` v

parse :: Line -> Instruction
parse (words -> s) = (op, sign, arg)
    where
        op = head s
        arg = read $ tail $ s !! 1
        sign = head $ s !! 1

solve :: [Line] -> Accumulator
solve (map parse -> b) = execute 0 b 0 []

main :: IO ()
main = interact $ show . solve . lines
