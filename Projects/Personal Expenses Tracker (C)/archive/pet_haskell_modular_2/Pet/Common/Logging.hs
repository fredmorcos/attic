module Pet.Common.Logging where

import System.IO

putErr :: String -> IO ()
putErr err = hPutStr stderr err

putErrLn :: String -> IO ()
putErrLn err = do
  errIsTerm <- hIsTerminalDevice stderr
  if errIsTerm then hPutStrLn stderr err else putErr err
