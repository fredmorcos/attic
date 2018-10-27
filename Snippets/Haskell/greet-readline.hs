module Main where

main = do putStrLn ("What is your name? ")
          input <- getLine
          putStrLn ("Hello " ++ input ++ ", what is your favorite color?")
          input <- getLine
          putStrLn ("I like " ++ input ++ " too!")