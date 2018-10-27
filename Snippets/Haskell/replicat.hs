module Main where

replicat :: Int -> a -> [a]
replicat 0 x = []
replicat n x = x:(replicat (n - 1) x)
