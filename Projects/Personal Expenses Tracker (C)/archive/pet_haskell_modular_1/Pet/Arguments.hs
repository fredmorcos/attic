module Pet.Arguments where

import Pet.Errors
import System.Console.GetOpt
import System.Environment
import System.Exit

data Options = Options { optShowVersion :: Bool
                       , optShowHelp    :: Bool
                       , optVerbose     :: Bool
                       , optAdd         :: [String]
                       , optRemove      :: [String]
                       , optNeedsFile   :: Bool
                       , optFiles       :: [String]
                       } deriving Show

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optShowHelp    = False
                         , optVerbose     = False
                         , optAdd         = []
                         , optRemove      = []
                         , optNeedsFile   = False
                         , optFiles       = []
                         }

version :: String
version = "pet 3.0"

parseArguments :: IO Options
parseArguments =
  case fmap parseOptions getArgs of
    Right argErr -> do
      putErr $ (show argErr) ++ usage
      flushErr
      exitFailure
    Left (opts, nopts) ->
      if optShowVersion opts
      then do putErrLn version
              exitSuccess
      else if optShowHelp opts
           then do putErrLn usage
                   exitSuccess
           else if optNeedsFile opts && nopts == []
                then do
                  putErr $ (show NoFileGivenError) ++ usage
                  exitFailure
                else return $ opts {optFiles = nopts}
  where header = "Usage: pet [OPTION...] FILE..."
        usage  = usageInfo header options

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['V'] ["version"]
    (NoArg (\o -> o {optShowVersion = True}))
    "Show version information"
  , Option ['h'] ["help"]
    (NoArg (\o -> o {optShowHelp = True}))
    "Show help"
  , Option ['v'] ["verbose"]
    (NoArg (\o -> o {optVerbose = True}))
    "Verbose output on stderr"
  , Option ['a'] ["add"]
    (ReqArg (\e o -> o {optAdd = (optAdd o) ++ [e],
                        optNeedsFile = True}) "EXP")
    "Add an expense element"
  , Option ['r'] ["remove"]
    (ReqArg (\e o -> o {optRemove = (optRemove o) ++ [e],
                        optNeedsFile = True}) "EXP")
    "Remove an expense element"
  ]

parseOptions :: [String] -> Either (Options, [String]) ArgumentError
parseOptions args = 
  if errs /= []
  then Right $ GetOptError errs
  else if uopts /= []
       then Right $ UnrecognizedOptError uopts
       else Left (foldl (flip id) defaultOptions opts, nopts)
  where (opts, nopts, uopts, errs) = getOpt' Permute options args
