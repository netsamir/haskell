module Parsing where

import Data.Char
import Control.Applicative
import Control.Monad

infixr 5 +++

-- The monad of parsers
--------------------

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance (Show a) => Show (Parser a) where
    show (Parser a) = "<Parser>"

instance Functor Parser where
    fmap f (Parser g) = Parser $ \inp -> case g inp of
                        [] -> []
                        [(v, out)] -> [(f v, out)]

instance Applicative Parser where
    pure v = Parser $ \inp -> [(v, inp)]
    (<*>) = ap

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \inp -> case p inp of
                    [] -> []
                    [(v, out)] -> let (Parser b) = f v
                                    in b out

instance MonadPlus Parser where
    mzero =  Parser $ \inp -> []
    mplus p q = Parser $ \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v,out)] -> [(v,out)]

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

-- Basic parsers
----------------

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

failure :: Parser a
failure = mzero

item :: Parser Char
item = Parser $ \inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)]

-- Choice
---------

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  p `mplus` q

-- Derived primitives
------------------

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\x -> if p x then return x else failure)
--sat p =  do x <- item
--           if p x then return x else failure

digit :: Parser Char
digit =  sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter =  sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] =  return []
string (x:xs) =  do char x
                    string xs
                    return (x:xs)

many' :: Parser a -> Parser [a]
many' p =  many1' p +++ return []

many1' :: Parser a -> Parser [a]
many1' p = do v <- p
              vs <- many' p
              return (v:vs)

ident :: Parser String
ident = do x <- lower
           xs <- many' alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1' digit
           return (read xs)

int :: Parser Int
int = error "You must implement int"

space:: Parser ()
space = do many (sat isSpace)
           return ()

comment :: Parser ()
comment = error "You must implement comment"
--
-- expr                          :: Parser Int
-- expr                          = error "You must implement expr"

-- Ignoring spacing
----------------

-- token                         :: Parser a -> Parser a
-- token p                       =  do space
--                                     v <- p
--                                     space
--                                     return v
--
-- identifier                    :: Parser String
-- identifier                    =  token ident
--
-- natural                       :: Parser Int
-- natural                       =  token nat
--
-- integer                       :: Parser Int
-- integer                       =  token int
--
-- symbol                        :: String -> Parser String
-- symbol xs                     =  token (string xs)

