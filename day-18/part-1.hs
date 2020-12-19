import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List

data Expr = Value Int | Add | Multiply | Paren [Expr] deriving Show

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do 
        (input',x) <- p input
        Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input,x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
       (input',f) <- p1 input
       (input'',a) <- p2 input'
       Just (input'',f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> 
        p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f 
    where 
        f (y:ys) 
            | y == x = Just (ys,x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
    let (token, rest) = span f input
    in Just (rest,token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do 
    (input', xs) <- p input
    if null xs 
        then Nothing
        else Just (input', xs)

value :: Parser Expr
value = f <$> notNull (spanP isDigit)
    where  
        f ds = Value $ read ds

paren :: Parser Expr
paren = Paren <$> (charP '(' *> inside <* charP ')')
    where inside = many expression

add :: Parser Expr
add = const Add <$> stringP "+"

multiply :: Parser Expr
multiply = const Multiply <$> stringP "*"

expression :: Parser Expr
expression = value <|> add <|> multiply <|> paren

check :: (Int,Expr) -> Expr -> (Int,Expr)
check (a,Add) (Value y) = let s = a + y in (s,Value s)
check (a,Multiply) (Value y) = let p = a * y in (p, Value p)
check (a,_) Add = (a,Add)
check (a,_) Multiply = (a,Multiply)
check (a,Add) (Paren x) = let i = evaluate x in (a + i, Value i)
check (a,Multiply) (Paren x) = let i = evaluate x in (a * i, Value i)

evaluate :: [Expr] -> Int
evaluate = fst . foldl check (0,Add)  

solve :: [String] -> Int
solve = sum . map (evaluate . snd . fromJust . runParser (many expression) . filter (/= ' '))

main :: IO () 
main = interact $ show . solve . lines
