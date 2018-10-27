module Main where

import System.Environment
import System.Exit
import Data.Char

data SpecialArgs = Help | Version

data Cmd = CmdCheck | CmdUnknown { cmd :: String } | CmdNone

petVersion, petDesc, petWWW, petCC, petAuthor, petEmail :: String
petVersion = "0.2"
petDesc    = "Personal Expense Tracker"
petWWW     = "https://github.com/fredmorcos/pet.git"
petCC      = "Copyright (c) 2012-2013"
petAuthor  = "Fred Morcos"
petEmail   = "fred.morcos@gmail.com"

specialArgs :: [String] -> Maybe SpecialArgs
specialArgs args =
  if not (any (\x -> x == "--help" || x == "-h") args)
  then if not (any (\x -> x == "--version" || x == "-V") args)
       then Nothing
       else Just Version
  else Just Help

printHelp :: IO ()
printHelp = do printVersion
               putStr $ unlines ["", "TODO: this is help"]

printVersion :: IO ()
printVersion =
  putStr $ unlines [unwords ["PET", petVersion, "--", petDesc],
                   petWWW, unwords [petCC, "-", petAuthor, "--",
                                    "<" ++ petEmail ++ ">"]]

strToLower :: String -> String
strToLower = map toLower

cmdParse :: [String] -> (Cmd, [String])
cmdParse [] = (CmdNone, [])
cmdParse xl@(x:xs) = if strToLower x == "check"
                     then (CmdCheck, xs)
                     else (CmdUnknown x, xl)

main :: IO ()
main = do args <- getArgs
          case specialArgs args of
            Nothing -> return ()
            Just Help -> printHelp >> exitSuccess
            Just Version -> printVersion >> exitSuccess
          params <- case cmdParse args of
            (CmdNone, _) ->
              putStrLn "No command given." >> exitFailure
            (CmdUnknown x, _) ->
              putStrLn ("Unknown command: " ++ x) >> exitFailure
            (CmdCheck, xs) ->
              putStrLn "You ran the CHECK command" >> return xs
