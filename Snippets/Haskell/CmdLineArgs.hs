import System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  mapM putStrLn args
  return ()
