module Main where

import System.IO ()
import Text.ParserCombinators.Poly.Plain

simple :: Parser Char Char
simple = next

main :: IO ()
main = do
  input <- getContents
  case runParser simple input of
   (Left  msg, _)         ->    putStrLn $ "Error: " ++ msg
   (Right res, remainder) -> do putStrLn $ "Result: " ++ show res
                                putStrLn $ "Remainder: " ++ remainder
  return ()
