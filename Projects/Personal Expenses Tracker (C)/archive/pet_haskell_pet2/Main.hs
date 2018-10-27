module Main where

import Command
import Expense
import Parser
import StringUtils
import System.Environment
import System.IO
import Data.List

seeHelp :: String
seeHelp = ", see " ++ quote "pet help"

execArgs :: [String] -> ([Expense] -> Result)
execArgs [] = error $ "No command given" ++ seeHelp
execArgs (x:xs) = case res of
  Nothing -> error $ "Unknown command " ++ quote x ++ seeHelp
  Just c  -> if length xs /= length (commandArgs c)
             then error $ "Bad number of arguments" ++ seeHelp
             else commandFunc c xs
  where res = find (\(Command n _ _ _) -> n == x) commandList

execResult :: Result -> IO ()
execResult (Error { message = m }) = do hPutStrLn stderr m
                                        hFlush stderr

main :: IO ()
main = do
  allArgs <- getArgs
  case allArgs of
    [] -> error $ "No arguments given" ++ seeHelp
    ("help":_) -> execResult $ execArgs allArgs []
    (file:args) -> case fmap getExpenses $ readFile file of
      Left e -> do error $ show e
      Right expenses -> do
        let cmd = execArgs args
            res = cmd expenses
        putStrLn res
  return ()
