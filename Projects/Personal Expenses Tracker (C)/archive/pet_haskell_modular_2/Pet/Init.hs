module Pet.Init where

import Pet.Common.Errors
import System.Directory
import System.IO.Error
import Control.Exception

petInit :: IO ()
petInit = (createDirectory "_pet") `catch` eHandler

eHandler :: IOError -> IO ()
eHandler err
  | isPermissionError    err = exitWith' noPerms
  | isAlreadyExistsError err = exitWith' alreadyPetRepo
  | otherwise                = exitWith' $ error' (show err) Nothing
