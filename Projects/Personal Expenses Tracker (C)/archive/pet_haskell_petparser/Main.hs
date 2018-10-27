module Main where

import           CommandLine
import           Control.Monad
import           Data.Char
import           Expense
import           Parser
import           PetParser
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do opts <- getArgs >>= parseCmdLine
          logF opts LogInfo "Parsed command-line arguments"
          contents <- input opts
          logF opts LogInfo "Read file contents"
          exps <- execParser opts contents
          _ <- execIOCmds exps $ cmds opts
          return ()
  where execIOCmds = foldM (\a f -> f a)

execParser :: Options -> String -> IO [Expense]
execParser opts cts =
  case runParser parseManyExpenses cts (1, 1) of
    Left (m, (l, c)) ->
      let msg = failedParse "Parse error" l c (Just m) cts
      in hPutStrLn stderr msg >> exitFailure
    Right (e, r, (l, c)) ->
      if not $ null r
      then let msg = failedParse "Incomplete parse" l c Nothing cts
           in hPutStrLn stderr msg >> exitFailure
      else logF opts LogInfo (successfulParse $ length e) >> return e
  where successfulParse l = "Successful parse: " ++ show l ++ " expenses"

pointerLine :: Int -> String
pointerLine c = replicate (c - 1) '-' ++ "^"

actualLine :: Int -> String -> String
actualLine l i = let linesList = lines i in if length linesList <= l - 1 then ""
                                            else linesList !! (l - 1)

failedParse :: String -> Int -> Int -> Maybe String -> String -> String
failedParse title l c msg cts = fullBadLine `seq`
  concat [ title, " at line ", show l, ", column ", show c,
           maybe "" (" -> " ++) msg, fullBadLine ]
  where badLine = dropWhile isSpace $ actualLine l cts
        fullBadLine = if null badLine then ""
                      else "\n  " ++ badLine ++ "\n--" ++ pointerLine c
