parse :: String -> [(Int,Int)]
parse [] = []
parse ('m':'u':'l':'(':rest) = 
    case reads rest of
        [(n1, ',':rest2)] -> 
            case reads rest2 of
                [(n2, ')':xs)] -> (n1, n2) : parse xs
                _ -> parse rest2
        _ -> parse rest
parse (_:xs) = parse xs

main :: IO ()
main = interact $ show . sum . map (uncurry (*)) . parse