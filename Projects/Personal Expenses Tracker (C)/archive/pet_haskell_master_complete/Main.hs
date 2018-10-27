module Main where

import Expense
import Pipeline
import Parser
import Errors
import Help
import System.Environment
import Data.Either
import Data.List

main :: IO ()
main = do
  args <- getArgs
  if args == [] then putNoCmd else case args of
    ("help":_) -> putErr showUsage
    _          -> do
      expL_ <- fmap parseExpenseList getContents
      expLRes <- putErrorLines expL_
      if expLRes then return () else do
        let expL = lefts expL_
        if expL == [] then putNoEntries else do
          let pl_ = createPL args expL
          plRes <- putErrors pl_
          if plRes then return () else do
            let pl = lefts pl_
                execPLRes = execPL expL pl
            case execPLRes of
              Left msg -> putErrDie msg
              Right res -> if res == [] then putNoEntries else do
                putStrLn $ showExpenseList $ sort res
  where putNoEntries = putErrDie $ show NoEntries
        putNoCmd = putErrDie $ show NoCmdGiven
