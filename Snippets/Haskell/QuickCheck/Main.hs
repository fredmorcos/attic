module Main where

import System.Environment
import System.IO

filenameFromArgs :: [String] -> String
filenameFromArgs []  = error "No file given"
filenameFromArgs [x] = x
filenameFromArgs _   = error "Too many arguments"

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  line <- getLine
  isEnd <- isEOF
  if isEnd
    then putStrLn "Leaving..."
    else do putStrLn line
            repl

main :: IO ()
main = do
  args     <- getArgs
  filename <- return $ filenameFromArgs args
  repl
