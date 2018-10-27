module Main where

import Pet.Parser
import Pet.Arguments
import Pet.Errors
import Pet.Expense
import Pet.Pipeline
import System.Exit
import System.IO.Error
import Data.Either
import Data.List
import Data.Time.LocalTime
import Control.Exception
import Text.Printf

main :: IO ()
main = do
  opts <- parseArguments
  verboseLog opts "finished parsing arguments"
  let fnames = optFiles opts
  verboseLog opts $ "files -> " ++ (intercalate ", " fnames)
  verboseLog opts "reading files..."
  fdatas <- mapM (\e -> catch (readFile e) exceptionHandler) fnames
  verboseLog opts "parsing files..."
  let expLists = map parseExpenseList fdatas
  verboseLog opts "checking for errors..."
  case checkErrors expLists of
    Just (i, err) -> do
      putErr $ show $ FileParserError (fnames !! i) err
      exitFailure
    Nothing -> return ()
  verboseLog opts "merging files..."
  let expList = concat $ lefts expLists
  verboseLog opts "creating processing pipeline..."
  verboseLog opts "executing pipeline..."
  return ()

verboseLog :: Options -> String -> IO ()
verboseLog opts str =
  if optVerbose opts
  then do zt <- getZonedTime
          lt <- return $ localTimeOfDay $ zonedTimeToLocalTime zt
          putErrLn $ unwords [printf "%-15s" $ show lt, "-- Log:", str]
  else return ()

exceptionHandler :: IOError -> IO String
exceptionHandler err = do
  putErrLn $ unwords ["Error:", getFilename, ioeGetErrorString err]
  exitFailure
  where getFilename = case ioeGetFileName err of
          Nothing -> "(Unknown)"
          Just x  -> x

checkErrors :: [Either [Expense] ParserError] -> Maybe (Int, ParserError)
checkErrors = _checkErrors 0
  where _checkErrors _ []     = Nothing
        _checkErrors n (x:xs) = case x of
          Left  _ -> _checkErrors (n + 1) xs
          Right y -> Just (n, y)
