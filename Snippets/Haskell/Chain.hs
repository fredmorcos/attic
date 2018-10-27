chain :: (Integral a) => a -> [a]
chain 1 = []
chain n | even n = n : chain (n `div` 2)
        | odd  n = n : chain (n * 3 + 1)

longChains :: Int
longChains = length (filter isLong [1..100])
           where isLong x = (length (chain x)) > 15

longChains' :: Int
longChains' = length (filter isLong (map chain [1..100]))
            where isLong x = length x > 15

longChains'' :: Int
longChains'' = length (filter (\x -> length x > 15) (map chain [1..100]))
