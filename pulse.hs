pulse :: [Integer] ->
pulse xs = foldl (f xs) [] ys
    where f xs = zipWith (-) (tail xs) xs 
