module Main where

import String

data Entry = Account String String String -- service, username, password

addEntry :: [Entry] -> Entry -> [Entry]
addEntry [] x = [x]

evalCmd :: [String] -> IO Bool
evalCmd [] = do return True
evalCmd (x:xs) =
  case x of
    "?" -> do putStrLn "Help"
              return True
    "a" -> do putStrLn "Add"
              return True
    "e" -> return False
    _   -> do putStrLn "Unknown"
              return True

main :: IO ()
main = do putStr ("> ")
          input <- getLine
          evalResult <- evalCmd (splitString input ' ')
          if (evalResult == True) then
             main
          else
             return ()
