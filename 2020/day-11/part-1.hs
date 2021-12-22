type Coordinate = (Int,Int)
type Seats = [String]
type Vacancy = Char

directions :: Coordinate -> Seats -> [Coordinate]
directions (x,y) s = filter (\(x,y) -> x >= 0 && x < length (head s) && y >= 0 && y < length s) adjacent
    where
        adjacent = [(x,up), (x,down),(left,up),(left,down),(left,y),(right,up),(right, down),(right,y)] 
        up = y - 1
        down = y + 1
        left = x - 1
        right = x + 1

check :: Coordinate -> Seats -> Vacancy
check (x,y) s 
    | c == '.' = '.'
    | l >= 4 = 'L'
    | l == 0 = '#'
    | otherwise = c
    where 
        c = s !! y !! x
        l = length $ filter (== True) $ map (\(x,y) -> s !! y !! x == '#') $ directions (x,y) s
        
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
