module Main (main) where

import System (getArgs)
import System.Console.GetOpt

data Flag = Version

options :: [OptDescr Flag]
options = [Option ['V'] ["version"] (NoArg Version) "Show version number"]

main = do args <- getArgs
          let (flags, nonOpts, msgs) = getOpt RequireOrder options args
          print $ length flags
