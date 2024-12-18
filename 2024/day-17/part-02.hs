module Main where

import Data.List (isPrefixOf)

-- Helper function to split on commas
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn sep [] = []
splitOn sep xs = 
    let (chunk, rest) = break (== head sep) xs
    in case rest of
        [] -> [chunk]
        (_:rest') -> if isPrefixOf sep rest
                     then chunk : splitOn sep (drop (length sep) rest)
                     else chunk : splitOn sep rest'

-- Parse input string to get program
parseProgram :: String -> [Int]
parseProgram = map read . splitOn ","

-- Calculate zine value from program
-- Process from right to left, since we want the rightmost number to be divided out first
calculateZine :: [Int] -> Int
calculateZine = (*8) . foldl (\acc x -> acc * 8 + x) 0 . reverse

-- Get sequence of outputs for a value
getOutputs :: Int -> [Int]
getOutputs n
    | n == 0 = []
    | otherwise = 
        let quotient = n `div` 8
            remainder = quotient `mod` 8
        in remainder : getOutputs quotient

-- Main function to handle input/output
main :: IO ()
main = do
    putStrLn "Enter program (comma-separated numbers):"
    input <- getLine
    let program = parseProgram input
    let zineValue = calculateZine program
    
    putStrLn $ "Calculated zine value: " ++ show zineValue
    putStrLn "Verification:"
    putStrLn $ "Original program: " ++ show program
    let outputs = takeWhile (const True) $ take (length program) $ getOutputs zineValue
    putStrLn $ "Output sequence: " ++ show outputs
    putStrLn $ "Matches program: " ++ show (outputs == program)
