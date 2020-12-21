{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.Printf

data Expr a =  Value a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            deriving (Functor)

instance Show a => Show (Expr a) where
    show (Value a) = show a
    show (Add l r) = printf "(%s + %s)" (show l) (show r)
    show (Mul l r) = printf "(%s * %s)" (show l) (show l)

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

spaces :: Parser String
spaces = spanP isSpace

lexeme :: Parser a -> Parser a 
lexeme p = p <* spaces

string :: String -> Parser String
string = traverse charP

symbol :: String -> Parser String
symbol = lexeme . string

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

between :: Parser a -> Parser a -> Parser (Expr Int) -> Parser (Expr Int)
between open close p = open *> p <* close

value :: Parser (Expr Int)
value = lexeme $ f <$> notNull (spanP isDigit)
    where
        f ds = Value $ read ds

parens :: Parser (Expr Int) -> Parser (Expr Int)
parens = between (symbol "(") (symbol ")")

expr :: Parser (Expr Int) 
expr = chainl1 term binOp
    where 
        term = value <|> parens expr
        binOp = addOp <|> mulOp
        addOp = Add <$ symbol "+"
        mulOp = Mul <$ symbol "*"

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = scan
    where
        scan = p <**> rest
        rest = (\f y g x -> g (f x y)) <$> op <*> p <*> rest <|> pure id

eval :: Num a => Expr a -> a
eval (Value x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

solve :: [String] -> Int
solve = sum . map (eval . snd . fromJust . runParser expr)

main :: IO ()
main = interact $ show . solve . lines
