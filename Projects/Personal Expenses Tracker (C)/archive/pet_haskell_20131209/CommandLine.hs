module CommandLine ( parseCommandLine
                   , interpretGlobalOptions
                   , defaultGlobalOptions
                   ) where

import           ListExtras
import           ProgramInfo
import           System.Console.GetOpt

data Flag = Verbose
          | Filename String
          | Output String
          | Help
          | Version
          | Display
          deriving Show

data GlobalOptions = GlobalOptions { verbose :: Bool
                                   , file    :: Maybe String
                                   , output  :: Maybe String
                                   } deriving Show

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions { verbose = False
                                     , file = Nothing
                                     , output = Nothing
                                     }

options :: [OptDescr Flag]
options =
  [ Option "v" ["verbose"] (NoArg  Verbose)         "Verbose output"
  , Option "f" ["file"]    (ReqArg Filename "FILE") "Input file"
  , Option "o" ["output"]  (ReqArg Output   "FILE") "Output file"
  , Option "V" ["version"] (NoArg  Version)         "Show version info"
  , Option "h" ["help"]    (NoArg  Help)            "Show help"
  , Option "d" ["display"] (NoArg  Display)         "Display all expenses"
  ]

parseCommandLine :: [String] -> Either String [Flag]
parseCommandLine args = case getOpt Permute options args of
  ([], [], []) -> Left $ displayErrors ["No arguments given\n"]
  (o, [], [])  -> Right o
  (_, ns, [])  -> Left $ displayErrors $ map (\o -> "Invalid option `" ++ o ++ "'\n") ns
  (_, _, es)   -> Left $ displayErrors es
  where displayErrors :: [String] -> String
        displayErrors strs = unlines [ "Errors:"
                                     , concatMap (" " ++) strs
                                     , usageInfo usageHeader options
                                     ]

interpretGlobalOption :: GlobalOptions -> Flag -> (GlobalOptions, Bool)
interpretGlobalOption gos f = case f of
  Verbose     -> (gos { verbose = True }, True)
  Filename fn -> (gos { file = Just fn }, True)
  Output   fn -> (gos { output = Just fn }, True)
  _           -> (gos, False)

interpretGlobalOptions :: (GlobalOptions, [Flag]) -> (GlobalOptions, [Flag])
interpretGlobalOptions (gos, fs) = foldFilter interpretGlobalOption gos fs []
