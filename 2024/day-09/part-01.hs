import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)

stringToDigits :: String -> [Int]
stringToDigits = map (\a -> if isDigit a then digitToInt a else 0)

-- Add up the result of multiplying each of these blocks' 
-- position with the file ID number it contains/
checksum :: String -> Int 
checksum x = result
  where 
    diskMap = stringToDigits x
    result = sum $ zipWith (*) diskMap [0..]
    
expand :: String -> String
expand x = concatMap expand' idToSpace
  where 
    idToSpace = zip [0..] x 
    expand' (id, c) = if (id + 1) `mod` 2 == 0 
                      then replicate (digitToInt c) '.' 
                      else replicate (digitToInt c) (intToDigit (min 9 (id `div` 2)))
                      
move :: String -> String 
move x 
  | isFull x = x 
  | otherwise = 
      case (leftMostSpaceIndex, rightmostDigitIndex) of
        (Just i, Just j) -> move $ swapChars i (length x - 1 - j) x
        _ -> x  -- handle case where indices aren't found
  where 
    isFull str = 
        let (nums, dots) = span (/= '.') str
        in all (== '.') dots
    rightmostDigitIndex = findIndex isDigit $ reverse x
    leftMostSpaceIndex = findIndex (== '.') x
    swapChars i j str = 
      let ci = str !! i
          cj = str !! j
      in replaceAt i cj (replaceAt j ci str)
    replaceAt n newChar str = take n str ++ newChar : drop (n+1) str
    

main :: IO () 
main = interact $ show . checksum . move . expand