module Pet.Errors where

import Extras.Data.String
import Text.Printf
import System.IO

putErr :: String -> IO ()
putErr = hPutStr stderr

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

flushErr :: IO ()
flushErr = hFlush stderr

data ArgumentError = GetOptError          [String]
                   | UnrecognizedOptError [String]
                   | NoFileGivenError

instance Show ArgumentError where
  show (GetOptError errs) =
    unlines $ ("Errors:":indent errs)
  show (UnrecognizedOptError nopts) =
    unlines $ ("Unrecognized options:":indent nopts) ++ [""]
  show NoFileGivenError =
    unlines $ ("Errors:":"  no file given":[""])

data ParserError = InvalidDateError     Int
                 | CannotParseLineError Int
                 | CannotParseDocError
                 | FileParserError      String ParserError

instance Show ParserError where
  show (InvalidDateError l) =
    printf "Error at line %d: invalid date" l
  show (CannotParseLineError l) =
    printf "Error at line %d: cannot parse" l
  show CannotParseDocError =
    printf "Error: cannot parse document"
  show (FileParserError fn err) =
    unlines $ [printf "Error in file %s" fn, "  " ++ show err]

data CommandError = CommandError String

instance Show CommandError where
  show (CommandError s) = s
