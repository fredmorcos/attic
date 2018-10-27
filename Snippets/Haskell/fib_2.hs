module Main where

fact :: Int -> Int
fact n = product [1..n]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do putStrLn $ "fact of " ++ show x ++ " is " ++ show (fact x)
          putStrLn $ "fib of "  ++ show x ++ " is " ++ show (fib  x)
  where x = 10
