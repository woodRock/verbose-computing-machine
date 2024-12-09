import Data.Char (digitToInt)
import Data.List (findIndex)

-- The '\100000' character is the 10000th ascii character
-- It will clash with nothing, and is used to represent the space character.
spaceChar :: Char
spaceChar = '\10000'

-- Add up the result of multiplying each of these blocks' 
-- position with the file ID number it contains.
checksum :: String -> Int 
checksum x = result
  where 
    diskMap = map (\a -> if a /= spaceChar then fromEnum a else 0) x
    result = sum $ zipWith (*) diskMap [0..]

-- Expand the dense format into the expanded format.
expand :: String -> String
expand x = concatMap expand' idToSpace
  where 
    idToSpace = zip [0..] x 
    expand' (id, c) = if (id + 1) `mod` 2 == 0 
                      then replicate (digitToInt c) spaceChar 
                      else replicate (digitToInt c) (toEnum (id `div` 2))
                      
move :: String -> String 
move x 
  | isFull x = x 
  | otherwise = 
      case (leftMostSpaceIndex, rightmostDigitIndex) of
        (Just i, Just j) -> move $ swapChars i (length x - 1 - j) x
        _ -> x  -- handle case where indices aren't found
  where 
    isFull str = 
        let (nums, dots) = span (/= spaceChar) str
        in all (== spaceChar) dots
    rightmostDigitIndex = findIndex (/= spaceChar) $ reverse x
    leftMostSpaceIndex = findIndex (== spaceChar) x
    swapChars i j str = 
      let ci = str !! i
          cj = str !! j
      in replaceAt i cj (replaceAt j ci str)
    replaceAt n newChar str = take n str ++ newChar : drop (n+1) str
    
main :: IO () 
main = interact $ show . checksum . move . expand