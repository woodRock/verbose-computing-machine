{-# LANGUAGE ViewPatterns #-}

import Control.Arrow
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

toMap :: [Food] -> Map Allergen (Set Ingredient)
toMap = foldl1 (M.unionWith S.intersection) . map go
    where
        go (i,a) = M.fromList $ zip a (repeat (S.fromList i))

solve :: [String] -> Int 
solve (parse -> foods) = length . concatMap (filter (`S.member` safeIngredients) . fst) $ foods
    where 
        ingredients = S.fromList . concatMap fst $ foods 
        possible = toMap foods 
        possibleAllergens = S.unions . M.elems $ possible
        safeIngredients = ingredients `S.difference` possibleAllergens

main :: IO ()
main = interact $ show . solve . lines 
