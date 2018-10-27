module Main where

import           CommandLine
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do args <- getArgs
          parsedArgs <-
            case parseCommandLine args of
              Left  err -> hPutStr stderr err >> exitFailure
              Right out -> return out
          print parsedArgs
          print $ interpretGlobalOptions (defaultGlobalOptions, parsedArgs)
