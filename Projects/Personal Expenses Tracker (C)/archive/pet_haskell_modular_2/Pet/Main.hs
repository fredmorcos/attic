module Main where

import Pet.Add
import Pet.Init
import Pet.Common.Errors
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    []          -> exitWith' noArgsGiven
    ("init":xs) -> petInit
    ("add":xs)  -> petAdd
    (x:xs)      -> exitWith' $ invalidArg x
