-- Encode using Caesar Cipher

import Data.Char


int2let :: Int -> Char
int2let n = toEnum (n + fromEnum 'a')::Char

let2int :: Char -> Int
let2int c = fromEnum c - fromEnum 'a'

encodeChar :: Int -> Char -> Char
encodeChar n c | Data.Char.isLower c = int2let ((let2int c + n) `mod` 26)
               | otherwise = c

encode :: Int -> String -> String
encode n xs = [encodeChar n c | c <- xs]

