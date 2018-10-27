module Main where

listconcat :: [a] -> [a] -> [a]
listconcat x []     = x
listconcat [] y     = y
listconcat (x:xs) y = x : (listconcat xs y)
