parse :: Bool -> String -> [(Int,Int)]
parse active [] = []
parse active ('d':'o':'(':')':xs) = parse True xs
parse active ('d':'o':'n':'\'':'t':'(':')':xs) = parse False xs
parse active ('m':'u':'l':'(':rest) = 
    case reads rest of
        [(n1, ',':rest2)] -> 
            case reads rest2 of
                [(n2, ')':xs)] -> (if active then (n1, n2) else (0,0)) : parse active xs
                _ -> parse active rest2
        _ -> parse active rest
parse active (_:xs) = parse active xs

main :: IO ()
main = interact $ show . sum . map (uncurry (*)) . parse True