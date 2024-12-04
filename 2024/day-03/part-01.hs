import Data.Char

-- Here is an example input:
-- xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
-- Only the four highlighted sections are real mul instructions. 
-- Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).
-- Look for the pattern mul(int,int) in the string and extract the two integers.
-- Multiply operations may be corrupted. If it doesn't follow the pattern keep going.
parse :: String -> [(Int,Int)]
parse [] = []
parse ('m':'u':'l':'(':x:',':a:')':xs) = (digitToInt x, digitToInt a) : parse xs
parse ('m':'u':'l':'(':x:',':a:b:')':xs) = (digitToInt x, read (a:[b])) : parse xs
parse ('m':'u':'l':'(':x:',':a:b:c:')':xs) = (digitToInt x, read (a:b:[c])) : parse xs
parse ('m':'u':'l':'(':x:y:',':a:')':xs) = (read (x:[y]), digitToInt a) : parse xs
parse ('m':'u':'l':'(':x:y:',':a:b:')':xs) = (read (x:[y]), read (a:[b])) : parse xs
parse ('m':'u':'l':'(':x:y:',':a:b:c:')':xs) = (read (x:[y]), read (a:b:[c])) : parse xs
parse ('m':'u':'l':'(':x:y:z:',':a:')':xs) = (read (x:y:[z]), digitToInt a) : parse xs
parse ('m':'u':'l':'(':x:y:z:',':a:b:')':xs) = (read (x:y:[z]), read (a:[b])) : parse xs
parse ('m':'u':'l':'(':x:y:z:',':a:b:c:')':xs) = (read (x:y:[z]), read (a:b:[c])) : parse xs
parse (_:xs) = parse xs

main :: IO ()
main = interact $ show . sum . map  (uncurry (*)). parse