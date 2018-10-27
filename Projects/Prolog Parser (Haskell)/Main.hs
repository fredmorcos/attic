module Main where

import System.IO
import System.Exit
import System.Environment

import SimpleParse
import PrologParse

import QueryHandler
import Printing

main :: IO ()
main = do
  contents <- getContents
  case runParser parseVarGroups contents of
   Left (remain, err) ->
     if not $ null remain
     then do
       hPutStrLn stderr $
         "Error parsing input, parsing incomplete: " ++ err
       exitFailure
     else do
       hPutStrLn stderr $
         "Parsing was complete but an error occurred: " ++ err
       exitFailure
   Right (remain, res) ->
     if not $ null remain
     then do
       hPutStrLn stderr $
         "Successful parsing, but incomplete: " ++ remain
       exitFailure
     else do
       args <- getArgs
       case executeQuery args res of
        Left err -> hPutStrLn stderr err
        Right queryRes -> putStrLn $ showPLStates queryRes
