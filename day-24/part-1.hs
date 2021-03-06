{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative

type Point = (Int,Int)
data Dir = E | SE | SW | W | NW | NE deriving (Show)

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

east :: Parser Dir
east = const E <$> symbol "e"

west :: Parser Dir
west = const W <$> symbol "w"

northEast :: Parser Dir
northEast = const NE <$> symbol "ne"

northWest :: Parser Dir
northWest = const NW <$> symbol "nw"

southEast :: Parser Dir
southEast = const SE <$> symbol "se"

southWest :: Parser Dir
southWest = const SW <$> symbol "sw"

direction = east <|> west <|> northEast <|> northWest <|> southEast <|> southWest

parse :: [String] -> [[Dir]]
parse = map go
    where go = snd . fromJust . runParser (many direction)

eval :: [Dir] -> Point
eval = foldl go origin
    where
        go (x,y) (dir -> (x',y')) = (x+x',y+y')
        origin = (0,0)

dir :: Dir -> Point
dir E = (1,0)
dir W = (-1,0)
dir NE = (1,1)
dir NW = (0,1)
dir SE = (0,-1)
dir SW = (-1,-1)

solve :: [String] -> Int
solve = length . filter odd . map length . group . sort . map eval . parse

main :: IO ()
main = interact $ show . solve . lines
