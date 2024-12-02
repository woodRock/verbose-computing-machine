import Data.List

-- Convert a space-separated string into an integer pair
readPair :: String -> (Int, Int)
readPair = tuple . map read . words
  where tuple [a,b] = (a,b)

-- Calculate total distance between sorted lists
computeDistance :: ([Int], [Int]) -> Int
computeDistance = sum . uncurry (zipWith ((abs .) . (-))) . both sort
  where both f (x,y) = (f x, f y)

main :: IO ()
main = interact $ show . computeDistance . unzip . map readPair . lines