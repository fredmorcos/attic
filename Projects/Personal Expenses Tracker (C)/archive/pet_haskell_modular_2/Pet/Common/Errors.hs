module Pet.Common.Errors where

import Pet.Common.Logging
import System.Exit

type Error = (String, Maybe Int)

error' :: String -> Maybe Int -> Error
error' msg ec = (msg, ec)

exitMessage :: Error -> String
exitMessage = fst

exitCode :: Error -> Maybe Int
exitCode = snd

exitWith' :: Error -> IO ()
exitWith' e = do putErrLn $ exitMessage e
                 case exitCode e of
                   Nothing -> exitFailure
                   Just ec -> exitWith $ ExitFailure $ ec

alreadyPetRepo :: Error
alreadyPetRepo = ("This directory is already a PET repository", Nothing)

notPetRepo :: Error
notPetRepo = ("This directory is not a PET repository", Nothing)

noPerms :: Error
noPerms = ("Not enough permissions to create PET repository", Nothing)

noArgsGiven :: Error
noArgsGiven = ("No arguments given", Nothing)

invalidArg :: String -> Error
invalidArg arg = ("Invalid argument `" ++ arg ++ "'", Nothing)

noSuchCmd :: String -> Int -> Error
noSuchCmd cmd ec = ("No such subcommand `" ++ cmd ++ "'", Just ec)

cmdNotFound :: String -> Int -> Error
cmdNotFound cmd ec = ("Cannot find executable `" ++ cmd ++ "'", Just ec)

cannotParse :: String -> Error
cannotParse cmd = ("Cannot parse output from `" ++ cmd ++ "'", Nothing)
