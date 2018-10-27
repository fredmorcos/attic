module Extras.Data.List where

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) | x `elem` xs = unique xs
              | otherwise   = (x:unique xs)
