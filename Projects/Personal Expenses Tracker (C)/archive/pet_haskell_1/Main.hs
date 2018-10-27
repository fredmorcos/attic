module Main where

import           System.Environment (getArgs, getProgName)

-- Available/possible sub-commands
data Command = Help | Check deriving Show

-- Parameters for each sub-command to parse and use as options
data CommandParams
  = HelpParams  { helpKeyword :: Maybe String }
  | CheckParams { inputData :: String }
  deriving Show

-- This is the verbosity level in general for the application,
-- including sub-commands like help, add, remove and check
data VerboseLevel = Quiet | Verbose | Debug deriving Show

-- These are general options that apply to the top-level of the
-- application and to sub-commands as well
data Opts = Opts { verbose :: VerboseLevel
                 , command :: Command
                 , params  :: CommandParams
                 } deriving Show

parseCommandLine :: Opts -> [String] -> Either String Opts
parseCommandLine opts args = 

main :: IO ()
main = do args <- getArgs
          prog <- getProgName
          putStrLn $ "hello" ++ unwords (map show args) ++ "foo" ++ prog
