module Errors where

import Extras
import Data.Either
import System.IO
import System.Exit

data ErrString =
  NoCmdGiven               |
  NoArgsGiven              |
  NoEntries                |
  NoEntriesOriginal        |
  NoEntriesFiltered        |
  UnknownCmd        String |
  ParserNothing            |
  ParserTooMany     String |
  ParserTooLittle   String |
  ParserErrAmount   String |
  ParserErrDate     String |
  ParserErrTags     String |
  MissingArgs       String |
  ExcessiveArgs     String |
  UnknownArg        String |
  ParserErrDates           |
  ParserErrMonths          |
  ParserErrYears           |
  PipelineEmpty

instance Show ErrString where
  show NoCmdGiven          = showErr "No command given"                True
  show NoArgsGiven         = showErr "No arguments given"              True
  show NoEntries           = showErr "No entries"                      False
  show (UnknownCmd c)      = showErr ("Unknown command: " ++ c)        True
  show ParserNothing       = showErr "Nothing to parse"                False
  show (ParserTooMany i)   = showErr ("Too many items: " ++ i)         False
  show (ParserTooLittle i) = showErr ("Too little items: " ++ i)       False
  show (ParserErrAmount a) = showErr ("Cannot parse amount: " ++ a)    False
  show (ParserErrDate d)   = showErr ("Cannot parse date: " ++ d)      True
  show (ParserErrTags t)   = showErr ("Cannot parse tags: " ++ t)      True
  show (MissingArgs c)     = showErr ("Missing arguments to: " ++ c)   True
  show (ExcessiveArgs c)   = showErr ("Excessive arguments to: " ++ c) True
  show (UnknownArg c)      = showErr ("Unknown argument to: " ++ c)    True
  show ParserErrDates      = showErr "Cannot parse dates"              True
  show ParserErrMonths     = showErr "Cannot parse months"             True
  show ParserErrYears      = showErr "Cannot parse years"              True
  show PipelineEmpty       = showErr "Pipeline is empty"               False
  show NoEntriesOriginal   = showErr "No entries in original list"     False
  show NoEntriesFiltered   = showErr "No entries in filtered list"     False

showErr :: String -> Bool -> String
showErr m h = "Error: " ++ m ++ if h then seeHelp else "."

seeHelp :: String
seeHelp = ". See `pet help'."

showLine :: (Show a, Show b) => (a, b) -> String
showLine (x, y) = "Line " ++ (show x) ++ ": " ++ (show y)

putErr :: String -> IO ()
putErr m = hPutStrLn stderr m

putErrDie :: String -> IO ()
putErrDie m = putErr m >> exitFailure

putErrors :: Show b => [Either a b] -> IO Bool
putErrors l | length lr > 0 = mapM_ (putErr . show) lr >> return True
            | otherwise     = return False
  where lr = rights l

putErrorLines :: (Show a, Show b) => [Either a b] -> IO Bool
putErrorLines l | length lr > 0 = mapM_ (putErr . showLine) lr >> return True
                | otherwise     = return False
  where ll = zip ([1..] :: [Integer]) l
        lr = map (\(x, Right y) -> (x, y)) $ filter (isRight . snd) ll
