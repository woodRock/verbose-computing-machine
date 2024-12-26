{-# LANGUAGE TupleSections #-}
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List (zip)

type Coord = (Int, Int)
type LegLengths = Map (Int, Char, Char) Int

calcFewest :: String -> Int -> Int
calcFewest code nRobotKeyboards = fewestPresses nRobotKeyboards code legLengths
  where
    -- Initial directional keypad coordinates
    initialKeyCoords :: Map Char Coord
    initialKeyCoords = Map.fromList $ concat 
        [[(c, (x, y)) | (x, c) <- zip [0..] row] | (y, row) <- zip [0..] [" ^A", "<v>"]]

    -- Initial leg lengths at layer 0
    initialLegLengths :: Map (Int, Char, Char) Int
    initialLegLengths = Map.fromList 
        [((0, ki, kf), 1) | ki <- Map.keys initialKeyCoords, kf <- Map.keys initialKeyCoords]

    -- Helper to calculate fewest presses for a sequence at a layer
    fewestPresses :: Int -> String -> LegLengths -> Int
    fewestPresses l ks legs = sum [legs ! (l, k1, k2) | (k1, k2) <- zip ('A':ks) ks]

    -- Calculate leg lengths for all layers
    legLengths :: LegLengths
    legLengths = foldl processLayer initialLegLengths [1..nRobotKeyboards]

    -- Process a single layer
    processLayer :: LegLengths -> Int -> LegLengths
    processLayer legs layer = 
        let keyCoords = if layer == nRobotKeyboards
                       then Map.fromList $ concat 
                            [[(c, (x, y)) | (x, c) <- zip [0..] row] 
                             | (y, row) <- zip [0..] ["789", "456", "123", " 0A"]]
                       else initialKeyCoords
            
            -- Calculate leg for a pair of keys
            calcLeg :: Char -> Char -> LegLengths -> LegLengths
            calcLeg ki kf legs =
                let (xi, yi) = keyCoords ! ki
                    (xf, yf) = keyCoords ! kf
                    blankPos = keyCoords ! ' '
                    horKs = replicate (abs (xf - xi)) (if xf > xi then '>' else '<')
                    verKs = replicate (abs (yf - yi)) (if yf < yi then '^' else 'v')
                    
                    fewestHorFirst = if (xf, yi) /= blankPos 
                                   then fewestPresses (layer-1) (horKs ++ verKs ++ "A") legs
                                   else maxBound
                    
                    fewestVerFirst = if (xi, yf) /= blankPos 
                                   then fewestPresses (layer-1) (verKs ++ horKs ++ "A") legs
                                   else maxBound
                    
                    minLen = min fewestHorFirst fewestVerFirst
                in Map.insert (layer, ki, kf) minLen legs

        -- Process all key pairs
        in foldr (\ki legs -> 
                 foldr (\kf legs -> 
                       calcLeg ki kf legs)
                       legs 
                       (Map.keys keyCoords))
                legs 
                (Map.keys keyCoords)

main :: IO ()
main = do
    input <- getContents
    let codes = lines input
    let part1 = sum [calcFewest code 3 * read (init code) | code <- codes]
    let part2 = sum [calcFewest code 26 * read (init code) | code <- codes]
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2