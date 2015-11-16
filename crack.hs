-- Cracking Caesar cipher encryption

import encode
import Data.Char

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

crack :: String -> Int
crack xs = encode (-factor) xs
    where
        factor = head (position (minimum chitab) chitab)
        chitab = [chiSquare (rotate n freqArg) freqRef| n <- [0 .. 25]]
        freqArg = freqAlpha xs
