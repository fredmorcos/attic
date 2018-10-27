module Main where

main = do x <- getContents
          putStrLn ("foobar " ++ (x) ++ " bazbazink")
