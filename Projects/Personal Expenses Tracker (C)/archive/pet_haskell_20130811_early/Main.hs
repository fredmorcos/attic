module Main where

import System.Environment

import Parser

data ParseArgsErr = NoArgs | TooManyArgs

main :: IO ()
main = do args <- getArgs
          case parseArgs args of
            Left NoArgs -> putStrLn "no arguments given."
            Right filename -> do filedata <- readFile filename
                                 let index = parsePersonIndexSection $ lines filedata
                                 return ()

parseArgs :: [String] -> Either ParseArgsErr String
parseArgs []  = Left NoArgs
parseArgs [x] = Right x
parseArgs _   = Left TooManyArgs
