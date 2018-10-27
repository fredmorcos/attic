module ArgsHandler where

import Data.Either

import SimpleParse
import PrologParse

parseArg :: String -> Either String PLVar
parseArg str = case runParser parseVar str of
  Left (remain, err) -> Left $ unlines [
    "Error parsing argument: " ++ err,
    "  Remaining input: " ++ remain]
  Right (remain, res) ->
    if not $ null remain
    then Left $ "Successful but incomplete parse of argument: " ++ remain
    else Right res

parseArgs :: [String] -> Either String PLState
parseArgs [] = Left "No query arguments given"
parseArgs args = let
  results = map parseArg args
  errors = lefts results :: [String]
  successes = rights results :: PLState
  in if null errors then Right successes else Left $ unlines errors
