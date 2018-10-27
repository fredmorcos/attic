module Main where

import CommandLine
import Control.Monad
import Data.Char
import Expense
import Parser
import PetParser
import System.Environment
import System.Exit
import System.IO

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
  case runParser parseManyExpenses cts [] (1, 1) (PosUpd updatePos) of
    Left (m, w, (l, c)) ->
      let msg = failedParse "Parse error" l c
                (if null w then Nothing else Just w)
                (if null m then Nothing else Just m) cts
      in hPutStr stderr msg >> exitFailure
    Right (e, r, w, (l, c), _) ->
      if not $ null r
      then let msg = failedParse "Incomplete parse" l c
                     (if null w then Nothing else Just w)
                     Nothing cts
           in hPutStr stderr msg >> exitFailure
      else logF opts LogInfo (successfulParse $ length e) >> return e
  where successfulParse l = "Successful parse: " ++ show l ++ " expenses"

failedParse :: String -> Int -> Int -> Maybe [String] -> Maybe [String] -> String -> String
failedParse t l c w m i =
  unlines $ [concat [t, " at line ", show l, ", column ", show c]]
  ++        maybe [] (map ("  %% " ++)) w
  ++        maybe [] (map ("  >> " ++)) m
  ++        maybe [] (const [""]) line
  ++        maybe [] (\x -> ["  " ++ dropWhile isSpace x]) line
  ++        maybe [] (const ["--" ++ replicate (c - 1) '-' ++ "^"]) line
  where line = let ls = lines i in if length ls <= l - 1 then Nothing
                                   else Just $ ls !! (l - 1)
