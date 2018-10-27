module Main where

accumList :: Num a => [a] -> a -> a
accumList [] y = y
accumList (x:xs) y = accumList xs (x + y)

main = do print (accumList [0..100000000] 0)
