import Data.Char (digitToInt)
import Data.List (findIndex, group, groupBy)
import Data.Maybe (fromMaybe, fromJust)

spaceChar :: Char
spaceChar = '\10000'

checksum :: String -> Int 
checksum x = result
  where 
    diskMap = map (\a -> if a /= spaceChar then fromEnum a else 0) x
    result = sum $ zipWith (*) diskMap [0..]

expand :: String -> (Char, String)
expand x = (maxId, result)
  where 
    idToSpace = zip [0..] x 
    expand' (id, c) = if (id + 1) `mod` 2 == 0 
                      then replicate (digitToInt c) spaceChar 
                      else replicate (digitToInt c) (toEnum (id `div` 2))
    result = concatMap expand' idToSpace
    maxId  = result !! (length result - 1)

moveFile :: (Char, String) -> String 
moveFile (id, x) = case getFilePos of
    Nothing -> x
    Just (start, end) ->
        let fileLen = end - start
            file = take fileLen $ drop start x
            
            -- Find all space sequences before current position
            spacesBeforeFile = 
                filter ((== spaceChar) . head) $ 
                groupBy (==) $ 
                take start x
                
            -- Calculate positions where spaces start
            spacePositions = scanl (+) 0 $ 
                map length $ 
                takeWhile (/= []) $ 
                groupBy (==) $ 
                take start x
                
            -- Find first space sequence that can fit file
            targetSpace = 
                case filter ((>= fileLen) . length) spacesBeforeFile of
                    [] -> Nothing
                    (s:_) -> Just $ head $ 
                            filter (\p -> take fileLen (drop p x) == replicate fileLen spaceChar) 
                            spacePositions
        in case targetSpace of
            Nothing -> x
            Just pos -> 
                take pos x ++                    -- Before file
                file ++                          -- The file
                take (start - pos - fileLen) (drop (pos + fileLen) x) ++  -- Middle content
                replicate fileLen spaceChar ++   -- Fill old position
                drop end x                       -- Rest of string
  where 
    getFilePos = do
        start <- findIndex (== id) x
        let revIndex = findIndex (== id) $ reverse x
        return (start, length x - fromJust revIndex)

move :: (Char, String) -> String
move (id, x) = case id of
    '\0' -> x
    _ -> move (nextId, moveFile (id, x))
    where nextId = toEnum (fromEnum id - 1)::Char

main :: IO () 
main = do
    input <- getContents
    let (maxId, expanded) = expand input
    putStrLn $ "Expanded: " ++ show expanded
    let final = move (maxId, expanded)
    putStrLn $ "Final: " ++ show final
    putStrLn $ "Checksum: " ++ show (checksum final)