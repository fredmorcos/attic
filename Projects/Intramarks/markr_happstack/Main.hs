module Main where

import Happstack.Lite
import qualified Templates as Templates
import qualified Settings as Settings

main :: IO ()
main = do
  putStrLn $ "Starting..."
  putStrLn $ "  " ++ Settings.name ++ ", Version " ++ (show Settings.version)
  putStrLn $ "  Running on " ++ Settings.host ++ ":" ++ (show Settings.port)
  serve Nothing app

-- app :: ServerPart Response
-- app = msum
--   [ dir "login" login
--   , dir "register" register
--   , index
--   ]

app :: ServerPart Response
app = msum
  [ index
  ]

index :: ServerPart Response
index = ok $ toResponse $ Templates.index
