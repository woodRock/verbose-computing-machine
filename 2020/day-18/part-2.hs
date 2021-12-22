{-# LANGUAGE DeriveFunctor #-}

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Printf

data Expr a =  Value a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            deriving (Functor)

instance Show a => Show (Expr a) where
    show (Value a) = show a 
    show (Add l r) = printf "(%s + %s)" (show l) (show r)
    show (Mul l r) = printf "(%s * %s)" (show l) (show l)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

intLiteral :: Parser Int
intLiteral = lexeme $ choice 
    [ char '-' >> fmap negate number
    , char '+' >> number
    , number
    ]
    where number = read <$> many1 digit

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a 
parens = between (symbol "(") (symbol ")")

eval :: Num a => Expr a -> a
eval (Value x) = x 
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

expr :: Parser (Expr Int)
expr = chainl1 factor mulOp
    where
        factor = chainl1 term addOp
        term = value <|> parens expr
        value = Value <$> intLiteral
        addOp = Add <$ symbol "+"
        mulOp = Mul <$ symbol "*"

parseInput :: String -> Expr Int
parseInput s = do 
    let result = parse expr "" s
    case result of
        Right v -> v 
        Left err -> Value 0

solve :: [String] -> Int
solve = sum . map (eval . parseInput)

main :: IO ()
main = interact $ show . solve . lines
