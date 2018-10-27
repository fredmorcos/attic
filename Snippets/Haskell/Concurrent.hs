module Main where

import           Control.Concurrent

main :: IO ()
main = do id1 <- forkIO (mapM_ (putStrLn . ("foo " ++) . show) ([1..100] :: [Int]))
          id2 <- forkIO (mapM_ (putStrLn . ("bar " ++) . show) ([1..100] :: [Int]))
          yield
          putStrLn $ show id1 ++ " " ++ show id2
