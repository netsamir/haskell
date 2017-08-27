-- Calculate the Newton-Raphson Square Roots

limit :: Float -> Float -> Float
limit app rac = (app + rac/app)/2

epsilon :: Float -> Float -> Float -> Float
epsilon s a e | abs(a - r) < e = r
              | abs(a - r) > e = epsilon s r e
              where r = limit a s

sqrt' :: Float -> Float
sqrt' n = epsilon n 1 0.00001

-- Prime Number

coprime' :: Int -> Int -> Bool
coprime' k c | k < c*c = True
             | k `mod` c == 0 = False
             | otherwise = coprime' k (c+2)

isprime' :: Int -> Bool
isprime' n | n < 2 = False
           | n == 2 = True
           | n `mod` 2 == 0 = False
           | otherwise = coprime' n 3

primes :: Int -> Int -> [Int]
primes n m | n > m = []
           | isprime' n = n:primes (n+1) m
           | otherwise = primes (n+1) m

-- Remove $,. from string
remove' :: String -> Char -> String
remove' xs c = [s | s <- xs, not (s == c)]

norm_num :: String -> String -> String
norm_num xs [] = xs
norm_num xs (c:xc) = norm_num smaller xc
                where smaller = remove' xs c

-- Remove $,. from string, Ex:
-- ghci> norm_num' "$1,245.99" "$,."
-- ghci> "124599"
norm_num' :: String -> String -> String
norm_num' xs [] = xs
norm_num' xs (c:xc) = norm_num' right xc
                where right = filter (/=c) xs

-- Review Chapter 10 - Declaring types and classes
type Pos = (Int, Int)
type Parser a = String -> [(a, String)]
type Assoc k v = [(k,v)]

find' :: Eq k => k -> Assoc k v -> v
find' k t = head[v| (k',v) <- t, k == k']

data Move = Left' | Right' | Up' | Down'
move :: Move -> Pos -> Pos
move Left' (x,y) = (x-1,y)

data Shape = Circle Float | Rect Float Float
square' :: Float -> Shape
square' n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ** 2
area (Rect x y) = x * y

-- Recursive data type
data Nat = Zero | Succ Nat
           deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

addn :: Nat -> Nat -> Nat
addn m n = int2nat(nat2int m + nat2int n)

addn' :: Nat -> Nat -> Nat
addn' Zero n = n
addn' (Succ  m) n = Succ (addn' m n)

data Tree = Leaf Int | Node Tree Int Tree
            deriving (Show)

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r

flaten :: Tree -> [Int]
flaten (Leaf n) = [n]
flaten (Node l n r) = flaten l ++ [n] ++ flaten r


-- Real Word Haskell book

count_char :: String -> Int
count_char xs = sum([length s | s <- words xs])

-- Data type
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazinInfo = Magazine Int String [String]
                   deriving (Show)

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody
type BookRecord = (BookInfo, BookReview)

myInfo = Book 9780135072455 "Algebra of Programming"
        ["Richard Bird", "Oege de Moor"]

-- Algebraic Data Types
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)
