ls_data = [1, 2, 3, 4, 5, 7, 8, 12]

plus_1 :: Integer -> Integer -> Bool
plus_1 x y = x == y - 1 || x == y

interm :: (Integer, Integer) -> Integer -> (Integer, Integer)
interm (x, y) n
    | plus_1 y n = (x, n)
    | otherwise = (n, n)

acc_red ::  [Integer] -> (Integer, Integer) -> [(Integer, Integer)]
acc_red [] (a, b) = [(a, b)]
acc_red (x:xs) (a, b)
    | base == (x, x) = (a, b) : acc_red xs base
    | otherwise = acc_red xs base
    where base = interm (a, b) x

create_intervals :: [Integer] -> [(Integer, Integer)]
create_intervals [] = []
create_intervals (x:xs) = tail(acc_red (x:xs) (x, x))
