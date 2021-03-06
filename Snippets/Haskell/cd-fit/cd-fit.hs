module Main where

import Text.ParserCombinators.Parsec

parseInput = do dirs <- many dirAndSize
                eof
                return dirs

data Dir = Dir Int String deriving Show

dirAndSize = do size <- many1 digit
                spaces
                dir_name <- anyChar `manyTill` newline
                return (Dir (read size) dir_name)

main = do input <- getContents
          -- putStrLn ("DEBUG: got input " ++ input)
          let dirs = case parse parseInput "stdin" input of
                          Left err -> error $ "Input:\n" ++ show input ++
                                              "\nError:\n" ++ show err
                          Right result -> result
          putStr "DEBUG: parsed: "
          print dirs
