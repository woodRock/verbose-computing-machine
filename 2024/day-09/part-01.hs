import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)

type Block = Either Int Char  -- Left Int for file ID, Right Char for dot

expand :: String -> [Block]
expand x = concatMap expand' idSpacePairs
  where 
    idSpacePairs = zip [0..] x 
    expand' (id, c) = 
      if (id + 1) `mod` 2 == 0 
      then replicate (digitToInt c) (Right '.')  -- free space
      else replicate (digitToInt c) (Left (id `div` 2))  -- file ID
                      
move :: [Block] -> [Block]
move x 
  | isFull x = x 
  | otherwise = 
      case (leftMostSpaceIndex, rightmostFileIndex) of
        (Just i, Just j) -> move $ swapBlocks i (length x - 1 - j) x
        _ -> x
  where 
    isFull blocks = 
        let (files, spaces) = span isFile blocks
        in all isDot spaces
    isFile (Left _) = True
    isFile _ = False
    isDot (Right '.') = True
    isDot _ = False
    rightmostFileIndex = findIndex isFile $ reverse x
    leftMostSpaceIndex = findIndex isDot x
    swapBlocks i j blocks = 
      let ci = blocks !! i
          cj = blocks !! j
      in replaceAt i cj (replaceAt j ci blocks)
    replaceAt n newBlock blocks = take n blocks ++ newBlock : drop (n+1) blocks

checksum :: [Block] -> Int 
checksum blocks = sum [i * id | (i, Left id) <- zip [0..] blocks]

main :: IO () 
main = interact $ show . checksum . move . expand