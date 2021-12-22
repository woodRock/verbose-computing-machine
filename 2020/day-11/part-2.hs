import Data.List

type Coordinate = (Int,Int)
type Direction = [Int]
type Seats = [String]
type Vacancy = Char

directions :: Coordinate -> Seats -> [String]
directions (x,y) s = map (map (\(dx,dy) -> s !! dy !! dx )) sanitized
    where
        sanitized = map (filter (\(x,y) -> x >= 0 && y >= 0 && x < length (head s) && y < length s)) directions
        directions = [l,r,u,d,pdu,pdd,sdu,sdd]
        l = zip [x - i | i <- xs] (repeat y)
        r = zip [x + i | i <- xs] (repeat y)
        u = zip (repeat x) [y - i | i <- ys]
        d = zip (repeat x) [y + i | i <- ys]
        pdu = zip [x + i | i <- xs] [y + i | i <- ys ]
        pdd = zip [x - i | i <- xs] [y - i | i <- ys ]
        sdu = zip [x + i | i <- xs] [y - i | i <- ys ]
        sdd = zip [x - i | i <- xs] [y + i | i <- ys ]
        xs = [1..length (head s) - 1] 
        ys = [1..length s - 1]

first :: String -> Bool
first [] = False
first (x:xs)
    | x == '.' = first xs
    | x == '#' = True 
    | x == 'L' = False

check :: Coordinate -> Seats -> Vacancy
check (x,y) s 
    | c == '.' = '.'
    | l >= 5 && c == '#' = 'L'
    | l == 0 && c == 'L' = '#'
    | otherwise = c
    where 
        c = s !! y !! x
        l = length $ filter (== True) $ map first $ directions (x,y) s
        
seats :: Seats -> Seats
seats s = map (\y -> map (\x -> check (x,y) s) [0..length (head s) - 1]) [0..length s - 1]

count :: Seats -> Int
count = sum . map (length . filter (== '#'))

solve :: Seats -> Int
solve s
    | s == n = count s
    | otherwise = solve n
    where n = seats s

main :: IO ()
main = interact $ show . solve . lines
