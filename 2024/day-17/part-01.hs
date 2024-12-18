import Debug.Trace
import Data.Bits

-- Types
type Program = [Int]
type Register = Int
type Memory = [Register]

-- OpCode definition
data OpCode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV 
    deriving (Eq)

instance Show OpCode where
    show ADV = "0"
    show BXL = "1"
    show BST = "2"
    show JNZ = "3"
    show BXC = "4"
    show OUT = "5"
    show BDV = "6"
    show CDV = "7"

-- Helper functions
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn sep [] = []
splitOn sep xs = 
    let (chunk, rest) = break (== head sep) xs
    in case rest of
        [] -> [chunk]
        (_:rest') -> if isPrefixOf sep rest
                     then chunk : splitOn sep (drop (length sep) rest)
                     else chunk : splitOn sep rest'
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

modifyAt :: Int -> Int -> [Int] -> [Int]
modifyAt i r xs = take i xs ++ [r] ++ drop (i + 1) xs

getComboValue :: Int -> Memory -> Int
getComboValue op memory
    | op <= 3   = op        -- literal values 0-3
    | op == 4   = memory!!0 -- register A
    | op == 5   = memory!!1 -- register B
    | op == 6   = memory!!2 -- register C
    | otherwise = error "Invalid combo operand 7"

-- Parsing
parse :: [String] -> (Memory, Program)
parse x = (regs, prog)
  where 
    (registers, _:program) = span (/= "") x
    regs = map (\register -> read $ drop 12 $ register) registers
    prog = map read $ splitOn "," $ drop 9 $ head $ program

-- Instruction execution
execute :: (Int, Int) -> Memory -> (Memory, Maybe Int)
execute (0, literal) memory = 
    let a = memory!!0
        combo = getComboValue literal memory
        result = a `div` (2^combo)
    in (modifyAt 0 result memory, Nothing)

execute (1, literal) memory = 
    let b = memory!!1
    in (modifyAt 1 (xor b literal) memory, Nothing)

execute (2, literal) memory = 
    let combo = getComboValue literal memory
        result = combo `mod` 8
    in (modifyAt 1 result memory, Nothing)

execute (3, literal) memory = (memory, Nothing)  -- Jump handling in runProgram

execute (4, literal) memory = 
    let (b,c) = (memory!!1, memory!!2)
    in (modifyAt 1 (xor b c) memory, Nothing)

execute (5, literal) memory = 
    let combo = getComboValue literal memory
        result = combo `mod` 8
    in (memory, Just result)

execute (6, literal) memory = 
    let a = memory!!0
        combo = getComboValue literal memory
        result = a `div` (2^combo)
    in (modifyAt 1 result memory, Nothing)

execute (7, literal) memory = 
    let a = memory!!0
        combo = getComboValue literal memory
        result = a `div` (2^combo)
    in (modifyAt 2 result memory, Nothing)

-- Program execution
runProgram :: Memory -> Program -> Int -> [Int] -> (Memory, [Int])
runProgram memory program pc outputs
    | pc >= length program - 1 = (memory, outputs)
    | otherwise = 
        let opcode = program!!pc
            literal = program!!(pc + 1)
            (newMemory, maybeOutput) = execute (opcode, literal) memory
            newOutputs = case maybeOutput of
                          Just x  -> outputs ++ [x]
                          Nothing -> outputs
            nextPc = case (opcode, memory!!0) of
                      (3, a) | a /= 0 -> literal
                      _               -> pc + 2
        in runProgram newMemory program nextPc newOutputs

solve :: (Memory, Program) -> String
solve (memory, program) = 
    let (_, outputs) = runProgram memory program 0 []
    in concat $ map show outputs

main :: IO () 
main = interact $ solve . parse . lines
