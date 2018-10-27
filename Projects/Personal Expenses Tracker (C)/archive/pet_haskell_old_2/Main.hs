module Main where

import           CommandLine
import           Commands
import           ErrorMessages
import           Expense
import           Parser
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

execParser :: Options -> String -> IO [Expense]
execParser opts contents = case runParser parseManyExpenses contents 1 1 of
  Left (m, l, c) -> do hPutStrLn stderr $ parseError l c m; exitFailure
  Right (e, r, l, c) ->
    if not $ null r
    then do hPutStrLn stderr $ incompleteParse l c; exitFailure
    else do logF opts LogInfo $ successfulParse $ length e; return e
