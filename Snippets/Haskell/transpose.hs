module Main where

createMatrix i j = 

printMatrix s = 

main = do putStrLn "Enter Matrix Size"
          matSize <- getLine
          let matrix = createMatrix matSize matSize
          printMatrix matSize
