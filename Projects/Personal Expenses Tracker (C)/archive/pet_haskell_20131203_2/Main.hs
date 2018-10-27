module Main where

import System.Environment
import CommandLine

main :: IO ()
main = do args <- getArgs
          progname <- getProgName
          case parseCommandLine progname args of
            Left err  -> putStrLn err
            Right out -> putStrLn $ show out
