import Data.List
import Data.List.Split 
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

parse :: String -> [(Integer, Integer)]
parse input = [ (-i, read b) | (i, b) <- zip [0..] $ splitOn "," input, b /= "x" ]

solve :: String -> Maybe Integer
solve = chineseRemainder . parse

main :: IO ()
main = interact $ show . solve . head . tail . lines
