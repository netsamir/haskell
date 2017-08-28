-- Test on folding

mySum :: [Integer] -> Integer
mySum = foldl (\acc x -> x + acc) 0

mySum' :: [Integer] -> Integer
mySum' = foldr (\x acc -> x + acc) 0

mySumScan = scanl (\acc x -> x + acc) 0

-- [1, 2, 3, 4] -> [(1,2), (2,3), (3, 4)]
withParam :: [Integer] -> [(Integer, Integer)]
withParam (x:y:[])  = [(x, y)]
withParam (x:y:xs) =  (x,y):withParam (y:xs)

-- withParam' :: [Integer] -> [(Integer, Integer)]
-- withParam' xs = foldl tuply []
--    where tuply acc x = (last acc)

--tuply :: Integer -> [Integer] -> ([Integer] -> (Integer, Integer))

-- https://stackoverflow.com/questions/27906660/does-haskells-foldr-always-take-a-two-parameter-lambda
compress' :: (Eq a) => [a] -> [a]
compress' xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs
-- compress' "aaaabccaadeeee"

compress'' :: Eq a => [a] -> [a]
compress'' [] = []
-- compress'' (z:zs) = z : foldr f (\_ -> []) zs z
compress'' (z:zs) = z : foldr f (const []) zs z
    where f x k = \w -> if x==w then k x else x : k x

compress''' xs = foldr f [last xs] xs
    where f x acc
            | x == (head acc) = acc
            | otherwise = x:acc
