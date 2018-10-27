module Main where

import Parser
import Index
import System.Environment
import Text.ParserCombinators.ReadP

data ParseArgsErr = NoArgs | TooManyArgs

type Pos = (Int, Int)

main :: IO ()
main = do args <- getArgs
          case parseArgs args of
            Left NoArgs -> putStrLn "no arguments given."
            Right filename -> do filedata <- readFile filename
                                 let parses = readP_to_S parseDocument filedata
                                 case indexFromParse filedata parses of
                                    Left (errStr, errPos) -> do
                                       putStr "Error:"
                                       putStrLn errStr
                                    Right idx -> return ()
                                 return ()

indexFromParse :: String -> [(Index, String)] -> Either (String, Pos) Index
indexFromParse _    (idx, remain) | null remain = Right idx
indexFromParse orig (_, remain) = Left (drop (length begin - 20) begin ++ take 20 remain)
  where begin = take (length orig - length remain) orig

parseArgs :: [String] -> Either ParseArgsErr String
parseArgs []  = Left NoArgs
parseArgs [x] = Right x
parseArgs _   = Left TooManyArgs
