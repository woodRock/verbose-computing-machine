import Data.List (sortBy, groupBy, sort)
import qualified Data.Map as M
import Data.Maybe

type Hand = String 
type Score = Int
type Bid = Int
type Rank = Int
type Type = Int

parse :: String -> (Hand, Bid)
parse x = (hand, score) 
    where
        [hand, tmp_score] = words x
        score = read tmp_score :: Int

getType :: Hand -> Type
getType hand
    | a == b && b == c && c == d && d == e || 
        a == b && b == c && c == d && e == 'J' || 
        a == b && b == c && d == 'J' && e == 'J' || 
        a == 'J' && b == 'J' && c == 'J' && d == e || 
        a == 'J' && b == 'J' && c == 'J' && d == 'J' 
        = 7 -- Five of a kind
    | a == b && b == c && c == d ||
        a == b && b == c && d == 'J' || 
        a == b && c == 'J' && d == 'J' || 
        a == b && d == 'J' && e == 'J' || 
        a == 'J' && b == 'J' && c == d || 
        a == 'J' && b == 'J' && c == 'J'
        = 6 -- Four of a kind
    | a == b && b == c && d == e || 
        a == b && b == c && e == 'J' || 
        a == b && c == 'J' && d == e || 
        a == b && c == d && e == 'J' 
        = 5 -- Full house
    | a == b && b == c ||     
        a == b && c == 'J' || 
        a == b && d == 'J' || 
        a == b && e == 'J' || 
        a == 'J' && b == 'J'
        = 4 -- Three of a kind
    | a == b && c == d
        = 3 -- Two pair
    | a == b || 
        a == 'J' || 
        b == 'J' || 
        c == 'J' || 
        d == 'J' || 
        e == 'J'
        = 2 -- One pair
    | otherwise 
        = 1 -- High card
    where 
        ordered_hand = (concat . 
                        sortBy (\a b -> compare (length b) (length a)) . 
                        groupBy (\a b -> a == b) . 
                        -- Sort by ranks 
                        sortBy (\a b -> compare (fromMaybe 0 (M.lookup a dict)) (fromMaybe 0 (M.lookup b dict)))
                        ) $ hand 
        [a, b, c, d, e] = ordered_hand
        ranks = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']
        dict = M.fromList $ 
            zip ranks
            (reverse [1 .. length ranks]) 
            
getRank :: Hand -> [Rank]
getRank hand = card_ranks
    where 
        ranks = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']
        dict = M.fromList $ 
            zip ranks
            (reverse [1 .. length ranks]) 
        card_ranks = map (\card -> fromMaybe 0 (M.lookup card dict)) hand

sortHandsByRank:: [Hand] -> [Hand]
sortHandsByRank xs = sortBy 
                    (\a b -> 
                        if (getType b) == (getType a)
                        then compare (getRank b) (getRank a)
                        else compare (getType b) (getType a)
                    ) xs

solve :: [(Hand, Score)] -> Int
solve xs = sum results
    where
        dict = M.fromList $ 
            zip (sortHandsByRank . map fst $ xs) -- Sort hands by rank
            (reverse $ [1 .. length xs]) -- Assign rank to each hand
        results = map (\(card, bid) -> fromMaybe 0 (M.lookup card dict) * bid) xs

main :: IO()
main = interact $ show . solve . map parse . lines 