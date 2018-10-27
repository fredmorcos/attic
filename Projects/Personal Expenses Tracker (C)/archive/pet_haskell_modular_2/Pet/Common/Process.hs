module Pet.Common.Process where

import Pet.Common.Errors
import System.Process
import System.Exit

type CommandReturn = Either String Error

executeCommand :: FilePath -> [String] -> String -> IO CommandReturn
executeCommand cmd args dat = do
  (ec, out, err) <- readProcessWithExitCode cmd args dat
  case ec of ExitFailure 127 -> return $ Right $ cmdNotFound cmd 127
             ExitFailure x   -> return $ Right $ error' err $ Just x
             ExitSuccess     -> return $ Left out
