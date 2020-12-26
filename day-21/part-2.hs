{-# LANGUAGE ViewPatterns #-}

import Control.Arrow
import Data.List (intercalate, sortBy, find)
import Data.Ord (comparing)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Ingredient = String
type Allergen = String
type Food = ([Ingredient], [Allergen])

parse :: [String] -> [Food]
parse = map go
    where 
        go = (i &&& a) . split
        i = words . head 
        a = words . filter (/= ',') . init . last
        split = splitOn "(contains "

possibleAllergens :: [Food] -> Map Allergen (Set Ingredient)
possibleAllergens = foldl1 (M.unionWith S.intersection) . map go
    where
        go (i,a) = M.fromList $ zip a (repeat (S.fromList i))

matchAllergens :: Map Allergen (Set Ingredient) -> [(Allergen, Ingredient)]
matchAllergens possible
    | M.null possible = []
    | otherwise = (allergen, ingredient) : matchAllergens rest
    where 
        rest = M.map (S.delete ingredient) . M.delete allergen $ possible 
        (allergen,ingredient) = extractSingletonSet . find ((==1) . S.size . snd) . M.toList $ possible
        extractSingletonSet (Just (a,i)) = (a, head . S.toList $ i)

solve :: [String] -> Map Allergen (Set Ingredient)
solve (parse -> foods) = possible
    where 
        ingredients = S.fromList . concatMap fst $ foods 
        possible = possibleAllergens foods 
        dangerous = intercalate "," . map snd . sortBy (comparing fst) . matchAllergens $ possible

main :: IO ()
main = interact $ show . solve . lines 
