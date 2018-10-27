module Main where

plusOne x = x + 1

addition x 0 = x
addition x 1 = plusOne x
addition x y = plusOne (addition x (y - 1))
