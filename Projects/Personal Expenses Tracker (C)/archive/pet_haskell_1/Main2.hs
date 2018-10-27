module Main where

import           System.Environment (getArgs, getProgName)

data VerboseLevel = Quiet | Verbose | Debug
data Command      = Help  | Check
data ParamsHelp   = ParamsHelp   { keyword   :: Maybe String
                                 , showAbout :: Bool
                                 }
data ParamsCheck  = ParamsCheck  { inputData :: String }
data OptsGeneral  = OptsGeneral  { verbose :: VerboseLevel
                                 , command :: Command
                                 }

main :: IO ()
main = do args <- getArgs
          prog <- getProgName
          putStrLn $ "hello" ++ unwords (map show args) ++ "foo" ++ prog
