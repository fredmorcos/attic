module Main where

import           System.Console.Haskeline

main :: IO ()
-- main = runInputT defaultSettings (handle (\Interrupt -> outputStrLn "Stopped.") $ withInterrupt loop)
--   where loop :: InputT IO ()
--         loop = do
--           minput <- getInputLine "% "
--           case minput of
--             Nothing -> return ()
--             Just "quit" -> return ()
--             Just input -> do outputStrLn $ "Input was: " ++ input
--                              loop
main = runInputT defaultSettings loop
  where loop :: InputT IO ()
        loop = handle do
          minput <- getInputLine "% "
          case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do outputStrLn $ "Input was: " ++ input
                             loop
