-- Cracking Caesar cipher encryption

import Data.Char


int2letLower :: Int -> Char
int2letLower n = toEnum (n + fromEnum 'a')::Char

int2letUpper :: Int -> Char
int2letUpper n = toEnum (n + fromEnum 'A')::Char

let2int :: Char -> Int
let2int c | Data.Char.isLower c = fromEnum c - fromEnum 'a'
          | Data.Char.isUpper c = fromEnum c - fromEnum 'A'

encodeChar :: Int -> Char -> Char
encodeChar n c | Data.Char.isLower c = int2letLower ((let2int c + n) `mod` 26)
               | Data.Char.isUpper c = int2letUpper ((let2int c + n) `mod` 26)
               | otherwise = c

encode :: Int -> String -> String
encode n xs = [encodeChar n c | c <- xs]

freqRef = []

numberOfLower :: String -> Int
numberOfLower xs = sum [1 | x <- xs, Data.Char.isLower x]

percentage :: Int -> Int -> Float
percentage n l = (fromIntegral n)/(fromIntegral l) * 100

freqsChar :: Char -> String -> Int
freqsChar c xs = sum [1 | x <- xs, x == c]

freqAlpha :: String -> [Float]
freqAlpha xs = [ percentage (freqsChar c xs) n | c <- ['a' .. 'z']]
    where n = numberOfLower xs

chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [((x - y)^2)/y | (x,y) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

position :: Eq a => a -> [a] -> [Int]
position c xs = [y | (x, y) <- zip xs [0 .. n], x == c]
    where
        n = length xs - 1
-- I have to correct the following
crack :: String -> [Char]
crack xs = encode (-factor) xs
    where
        factor = head (position (minimum chitab) chitab)
        chitab = [chiSquare (rotate n freqArg) freqRef| n <- [0 .. 25]]
        freqArg = freqAlpha xs

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = p x && all p xs

allfr :: (a -> Bool) -> [a] -> Bool
allfr p = foldr (\ x y -> p x && y) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr(\ x y -> p x || y) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x:takeWhile' p xs
                    | otherwise = []

takeWhilefr :: (a -> Bool) -> [a] -> [a]
takeWhilefr p = foldr (\ x y -> if p x then x : y else [] ) []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

folddropW :: (a -> Bool) -> [a] -> [a]
folddropW p = foldr(\ x y -> if p x then y else x:y ) []

sum' :: [Int] -> Int
sum' = sum'' 0
    where
        sum'' v [] = v
        sum'' v (x:xs) = sum''(v + x) xs

reverse' :: [a] -> [a]
reverse' = foldr (\ x y -> y ++ [x]) []

reversel :: [a] -> [a]
reversel = foldl (\x y -> y:x) []

dropWhilel :: (a -> Bool) -> [a] -> [a]
dropWhilel p [] = []
dropWhilel p (x:xs) | p x = dropWhilel p xs
                    | otherwise = x:xs

foldLW  :: (a -> Bool) -> [a] -> [a]
foldLW p = foldl(\ x y -> if p y then x else y:x ) []

sumsq = compose' [ \ a -> a + 1, \ a -> a * 2 ]

compose' :: [Int -> Int] -> (Int -> Int)
compose' = foldr (.) id

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \ x y -> f (x, y)

samir :: (Integer, Integer) -> Integer
samir (a, b) = a + b

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \ (x, y) -> f x y

samir' :: Integer -> Integer -> Integer
samir' a b = a + b

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

