import qualified Data.Set as Set

data Robot = Robot
  { posX :: Int
  , posY :: Int
  , velX :: Int
  , velY :: Int
  } deriving (Show, Eq)

-- Parse input line of form "p=x,y v=dx,dy"
parseRobot :: String -> Robot
parseRobot line = 
  let (pos, vel) = break (== 'v') line
      (px, py) = parseCoords pos
      (vx, vy) = parseCoords vel
  in Robot px py vx vy

parseCoords :: String -> (Int, Int)
parseCoords s = 
  let (x, rest) = span (/= ',') (dropWhile (not . isDigit) s)
      y = takeWhile isDigit (drop 1 rest)
  in (parseInt x, parseInt y)
  where
    isDigit c = c >= '0' && c <= '9' || c == '-'

parseInt :: String -> Int
parseInt s = case reads s of
  [(n, "")] -> n
  _ -> error "Invalid integer"

moveRobot :: Robot -> Robot
moveRobot (Robot px py vx vy) = Robot newX newY vx vy
  where
    newX = ((px + vx) `mod` gridWidth + gridWidth) `mod` gridWidth
    newY = ((py + vy) `mod` gridHeight + gridHeight) `mod` gridHeight

gridWidth :: Int
gridWidth = 101

gridHeight :: Int
gridHeight = 103

-- Get all positions as (x,y) pairs for a list of robots
getRobotPositions :: [Robot] -> Set.Set (Int, Int)
getRobotPositions = Set.fromList . map (\r -> (posX r, posY r))

-- Check if all positions are unique
hasUniquePositions :: [Robot] -> Bool
hasUniquePositions robots =
  let positions = getRobotPositions robots
  in Set.size positions == length robots

-- Create grid representation
createGrid :: [Robot] -> String
createGrid robots = unlines [row y | y <- [0..gridHeight-1]]
  where
    positions = getRobotPositions robots
    row y = [if Set.member (x, y) positions then '#' else '.' | x <- [0..gridWidth-1]]

-- Simulate until unique positions found or max steps reached
findUniquePositions :: [Robot] -> Maybe ([Robot], Int)
findUniquePositions initialRobots = go initialRobots 0
  where
    maxSteps = 1000000  -- Limit search to prevent infinite loop
    go robots step
      | step > maxSteps = Nothing
      | hasUniquePositions robots = Just (robots, step)
      | otherwise = go (map moveRobot robots) (step + 1)

main :: IO ()
main = do
  input <- getContents
  let initialRobots = map parseRobot $ lines input
  putStrLn $ "Simulating " ++ show (length initialRobots) ++ " robots..."
  case findUniquePositions initialRobots of
    Nothing -> putStrLn "No unique positions found within maximum steps"
    Just (robots, steps) -> do
      putStrLn $ "Found unique positions after " ++ show steps ++ " steps:"
      putStrLn $ createGrid robots

-- Example usage:
-- cat input.txt | runhaskell solution.hs