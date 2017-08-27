{- Start the study of the monads through a parser.

A monad is defined by three things:
 1) A type constructor
 2) a function return
 3) an operator (==>) which is pronounced "bind"
-}

import Control.Applicative()
import Control.Monad (liftM, ap)

-- 1) A type constructor

newtype Parser' a = P' (String -> [(a, String)])

--  2) Basic functions
instance Monad Parser' where

return' v = P'(\inp -> [(v,inp)])

-- >>= :: Parser' a -> (a -> Parser' b) -> Parser' b
p >>= q = P'(\inp -> case parser' p inp of
    [] -> []
    [(v,out)] -> parser' (q v) out)

instance MonadPlus Parser' where
    mzero = P (\inp -> [])
    p `mplus` q = P (\inp -> case parsr p inp of
                        [] -> parse q inp
                        [(v,out)] -> [(v, out)])

failure' :: Parser' a
failure' = P'(\inp -> [])

items' :: Parser' Char
items' =  P'(\inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)])

-- 2.1) parser

parser' :: Parser' a -> String -> [(a, String)]
parser' (P' p) inp = p inp


-- 3.1) Sequencing

{-

-- 3.2) Choice

(+++) :: Parser' a -> Parser' a -> Parser' a
(+++) p q = \inp -> case parser' p inp of
    [] -> parser' p inp
    [(v, out)] -> [(v, out)]

{- End of Monad definition -}

--  4) Satisfy Parsers

sat' :: (Char -> Bool) -> Parser' Char
sat' p = do x <- items'
            if p x then return' x else failure'

-- 5) Derived functions
-}
