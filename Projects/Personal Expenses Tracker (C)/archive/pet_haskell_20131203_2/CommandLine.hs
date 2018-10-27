module CommandLine (parseCommandLine) where

import System.Console.GetOpt

data Flag = Verbose
          | Help
          | Version
          | Filename String
          | Output String
          | Command (Maybe PetCommand)
          deriving Show

data PetCommand = Create
                | Display
                deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg  Verbose)             "Verbose output"
  , Option ['V'] ["version"] (NoArg  Version)             "Show version info"
  , Option ['h'] ["help"]    (NoArg  Help)                "Show help"
  , Option ['f'] ["file"]    (ReqArg Filename     "FILE") "Input file"
  , Option ['o'] ["output"]  (ReqArg Output       "FILE") "Output file"
  , Option ['c'] ["command"] (ReqArg strToCommand "CMD")  "Command to execute"
  ]

strToCommand :: String -> Flag
strToCommand str = case str of
  "create"  -> Command $ Just Create
  "display" -> Command $ Just Display
  _         -> Command $ Nothing

parseCommandLine :: String -> [String] -> Either String ([Flag], [String])
parseCommandLine progname args = case getOpt Permute options args of
  (o, n, [])   -> Right (o, n)
  (_, _, errs) -> Left $ concat errs ++ usageInfo header options
  where header = "Usage: " ++ progname ++ " [OPTIONS]"
