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

getType :: Hand -> Int
getType hand
    | a == b && b == c && c == d && d == e || 
        a == b && b == c && c == d && e == 'J' || 
        a == b && b == c && d == 'J' && e == 'J' || 
        a == 'J' && b == 'J' && c == 'J' && d == e || 
        a == 'J' && b == 'J' && c == 'J' && d == 'J' 
        = 7 -- Five of a kind
        -- = (7, ordered_hand)
    | a == b && b == c && c == d ||
        a == b && b == c && d == 'J' || 
        a == b && c == 'J' && d == 'J' || 
        a == b && d == 'J' && e == 'J' || 
        a == 'J' && b == 'J' && c == d || 
        a == 'J' && b == 'J' && c == 'J'
        = 6 -- Four of a kind
        -- = (6, ordered_hand)
    | 
        a == b && b == c && d == e || 
        a == b && b == c && e == 'J' || 
        a == b && c == 'J' && d == e || 
        a == b && c == d && e == 'J' 
        = 5 -- Full house
        -- = (5,ordered_hand) 
    | 
        a == b && b == c ||     
        a == b && c == 'J' || 
        a == b && d == 'J' || 
        a == b && e == 'J' || 
        a == 'J' && b == 'J'
        = 4 -- Three of a kind
        -- = (4, ordered_hand)
    | 
        a == b && c == d || 
        a == b && e == 'J' ||
        a == 'J' && b == c
        = 3 -- Two pair
        -- = (3, ordered_hand)
    | 
        a == b || 
        a == 'J' || 
        b == 'J' || 
        c == 'J' || 
        d == 'J' || 
        e == 'J'
        = 2 -- One pair
        -- = (2, ordered_hand)
    | otherwise 
        = 1 -- High card
        -- = (1, ordered_hand)
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

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

-- Five of a kind
test1 = assert (getType "AAAAA" == 7) "T1: five of a kind"
test2 = assert (getType "KKKKK" == 7) "T2: five of a kind"
-- Four of a kind 
test3 = assert (getType "KKKKA" == 6) "T3: four of a kind"
test4 = assert (getType "AAAA2" == 6) "T4: four of a kind"
-- Full house 
test5 = assert (getType "KKKAA" == 5) "T5: full house"
test6 = assert (getType "KKKQQ" == 5) "T6: full house"
-- Three of a kind
test7 = assert (getType "KKK23" == 4) "T7: three of a kind"
test8 = assert (getType "AAA23" == 4) "T8: three of a kind"
-- Two pair
test9 = assert (getType "KK224" == 3) "T9: two pair"
test10 = assert (getType "AA334" == 3) "T10: two pair"
-- One Pair
test11 = assert (getType "KK234" == 2) "T11: one pair"
test12 = assert (getType "AA234" == 2) "T12: one pair"
-- High card 
test13 = assert (getType "KQ234" == 1) "T13: high card"
test14 = assert (getType "AK234" == 1) "T14: high card"

-- Comparison tests.
test15 = assert (sortHandsByRank ["AAAAA", "KK8KK"] == ["AAAAA", "KK8KK"]) "C1: Greater"
test16 = assert (sortHandsByRank ["KKKKK", "QQQQ4"] == ["KKKKK", "QQQQ4"]) "C2: Greater"
test17 = assert (sortHandsByRank ["QQQQQ", "QQQQQ"] == ["QQQQQ", "QQQQQ"]) "C3: Equal"
test18 = assert (sortHandsByRank ["QQQQQ", "KKKKK"] == ["KKKKK", "QQQQQ"]) "C4: Less"
test19 = assert (sortHandsByRank ["TTTTT", "KKKKK"] == ["KKKKK", "TTTTT"]) "C5: Less"
test20 = assert (sortHandsByRank ["8KQAJ", "9A234"] == ["8KQAJ", "9A234"]) "C6: Less"

-- Joker wild cards.
-- Five of a kind
test21 = assert (getType "KKKKJ" == 7) "J1: five of a kind"
test22 = assert (getType "KKKJJ" == 7) "J2: five of a kind"
test23 = assert (getType "KKJJJ" == 7) "J3: five of a kind"
test24 = assert (getType "KJJJJ" == 7) "J4: five of a kind"
test25 = assert (getType "JJJJJ" == 7) "J5: five of a kind"
-- Four of kind 
test26 = assert (getType "KKKJ2" == 6) "J6: ffour of a kind"
test27 = assert (getType "KKJJ2" == 6) "J7: four of a kind"
test28 = assert (getType "KJJJ2" == 6) "J8: four of a kind"
test29 = assert (getType "QJJQ2" == 6) "J9: four of a kind"
test30 = assert (getType "T55J5" == 6) "J10: four of a kind"
test31 = assert (getType "QQQJA" == 6) "J11: four of a kind"
-- Full house 
test32 = assert (getType "KKJAA" == 5) "J12: full house"
test33 = assert (getType "KKJQQ" == 5) "J13: full house"
test34 = assert (getType "JKKQQ" == 5) "J14: full house"
-- Three of a kind 
test35 = assert (getType "KKJ23" == 4) "J15: three of a kind"
test36 = assert (getType "KJJ23" == 4) "J16: three of a kind"
test37 = assert (getType "JJA23" == 4) "J17: three of a kind"
-- Two pair
-- Any two pair examples with joker, would become three of a kind.
-- One pair 
test38 = assert (getType "KJ234" == 2) "J20: one pair"
test39 = assert (getType "JA234" == 2) "J21: one pair"

main :: IO()
-- Run all the tests. 
main = do
    putStrLn test1
    putStrLn test2
    putStrLn test3
    putStrLn test4
    putStrLn test5
    putStrLn test6
    putStrLn test7
    putStrLn test8
    putStrLn test9
    putStrLn test10
    putStrLn test11
    putStrLn test12
    putStrLn test13
    putStrLn test14
    putStrLn test15
    putStrLn test16
    putStrLn test17
    putStrLn test18
    putStrLn test19
    putStrLn test20
    putStrLn test21
    putStrLn test22
    putStrLn test23
    putStrLn test24
    putStrLn test25
    putStrLn test26
    putStrLn test27
    putStrLn test28
    putStrLn test29
    putStrLn test30
    putStrLn test31
    putStrLn test32
    putStrLn test33
    putStrLn test34
    putStrLn test35
    putStrLn test36
    putStrLn test37
    putStrLn test38
    putStrLn test39
