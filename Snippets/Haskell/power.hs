module Main where

power x 0 = 1
power x 1 = x
power x y = (power x (y - 1)) * x
