-- Types
data Robot = Robot
  { posX :: Int
  , posY :: Int
  , velX :: Int
  , velY :: Int
  } deriving (Show)

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Show, Eq, Enum)

-- Constants
gridWidth :: Int
gridWidth = 101

gridHeight :: Int
gridHeight = 103

simulationTime :: Int
simulationTime = 100

-- String parsing helpers
parseInt :: String -> Int
parseInt s = case reads s of
  [(n, "")] -> n
  _ -> error "Invalid integer"

parseCoords :: String -> (Int, Int)
parseCoords s = 
  let (x, rest) = span (/= ',') (dropWhile (not . isDigit) s)
      y = takeWhile isDigit (drop 1 rest)
  in (parseInt x, parseInt y)
  where
    isDigit c = c >= '0' && c <= '9' || c == '-'

-- Parse input line of form "p=x,y v=dx,dy"
parseRobot :: String -> Robot
parseRobot line = 
  let (pos, vel) = break (== 'v') line
      (px, py) = parseCoords pos
      (vx, vy) = parseCoords vel
  in Robot px py vx vy

-- Move a robot one step, handling wrapping at grid boundaries
moveRobot :: Robot -> Robot
moveRobot (Robot px py vx vy) = Robot newX newY vx vy
  where
    newX = ((px + vx) `mod` gridWidth + gridWidth) `mod` gridWidth
    newY = ((py + vy) `mod` gridHeight + gridHeight) `mod` gridHeight

-- Simulate movement for all robots for given number of steps
simulateRobots :: [Robot] -> Int -> [Robot]
simulateRobots robots steps = iterate (map moveRobot) robots !! steps

-- Determine which quadrant a robot is in (if not on middle lines)
getQuadrant :: Robot -> Maybe Quadrant
getQuadrant (Robot x y _ _)
  | x == midX || y == midY = Nothing
  | x < midX && y < midY = Just TopLeft
  | x > midX && y < midY = Just TopRight
  | x < midX && y > midY = Just BottomLeft
  | otherwise = Just BottomRight
  where
    midX = gridWidth `div` 2
    midY = gridHeight `div` 2

-- Count robots in each quadrant
countQuadrants :: [Robot] -> [Int]
countQuadrants robots = map count [TopLeft .. BottomRight]
  where
    quadrants = [q | Just q <- map getQuadrant robots]  -- List comprehension instead of mapMaybe
    count q = length $ filter (== q) quadrants

-- Calculate safety factor (product of all quadrant counts)
calculateSafetyFactor :: [Robot] -> Int
calculateSafetyFactor = product . countQuadrants

-- Main solution
solve :: String -> Int
solve input = calculateSafetyFactor finalPositions
  where
    initialRobots = map parseRobot $ lines input
    finalPositions = simulateRobots initialRobots simulationTime

main :: IO ()
main = do
  input <- getContents
  let result = solve input
  putStrLn $ "Safety factor: " ++ show result

-- Example usage:
-- cat input.txt | runhaskell solution.hs