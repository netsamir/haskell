-- Encode using Caesar Cipher

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

