module CommandLine where

import           Commands
import           Control.Monad
import           Data.List
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

data Options = Options { input  :: IO String
                       , output :: String -> IO ()
                       , logF   :: LogLevel -> String -> IO ()
                       , cmds   :: [Cmd]
                       }

data LogLevel = LogInfo | LogWarn | LogError deriving (Eq, Ord)

defaultOptions :: Options
defaultOptions = Options { input  = getContents
                         , output = putStr
                         , logF   = outF LogWarn
                         , cmds   = []
                         }

updateCmds :: Options -> Cmd -> Options
updateCmds opts cmd = opts { cmds = cmds opts ++ [cmd] }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "v" ["verbose"] (NoArg  readVerbose       ) "Also output status"
  , Option "q" ["quiet"]   (NoArg  readQuiet         ) "Only output errors"
  , Option "o" ["output"]  (ReqArg readOutput  "FILE") "Output file [stdout]"
  , Option "V" ["version"] (NoArg  readVersion       ) "Show version info"
  , Option "h" ["help"]    (NoArg  readHelp          ) "Show help"
  , Option "d" ["display"] (NoArg  readDisplay       ) "Display all expenses"
  , Option "p" ["person"]  (ReqArg readPerson  "NAME") "Filter by person name" ]
  where readVerbose opts = return opts { logF = outF LogInfo }
        readQuiet   opts = return opts { logF = outF LogError }
        readDisplay opts = return $ updateCmds opts displayExpenses
        readPerson arg opts = return $ updateCmds opts $ selectPerson arg
        readOutput  arg opts =
          return $ if arg == "-"
                   then opts { output = output defaultOptions }
                   else opts { output = writeFile arg }
        readVersion _ = do progName >>= \pname -> putStr $ versionInfo pname
                           exitSuccess
        readHelp    _ = do pname <- progName
                           putStrLn $ versionInfo pname
                           putStr $ usageInfo (usageHeader pname) options
                           exitSuccess

outF :: LogLevel -> LogLevel -> String -> IO ()
outF minLevel level msg =
  unless (minLevel > level) $ hPutStrLn stderr $
  (case level of LogInfo  -> "[LOG] "
                 LogWarn  -> "[WRN] "
                 LogError -> "[ERR] ") ++ msg

readFiles :: [FilePath] -> IO String
readFiles = foldM (\a v -> readFile v >>= (\d -> return $ a ++ d)) []

parseCmdLine :: [String] -> IO Options
parseCmdLine args = case getOpt Permute options args of
  ([], [], []) -> return defaultOptions
  (os, [], []) -> foldOpts os
  (os, ns, []) -> foldOpts os >>= (\o -> return o { input = readFiles ns })
  (_,   _, es) -> do hPutStrLn stderr $
                       if length es > 1 then "Errors: " else "Error: "
                     mapM_ (hPutStr stderr . ("  " ++)) es
                     hPutStrLn stderr ""
                     pname <- progName
                     hPutStr stderr $ usageInfo (usageHeader pname) options
                     exitFailure
  where foldOpts = foldl (>>=) (return defaultOptions)

progName :: IO String
progName = do pname <- getProgName
              return $ if "pet" `isInfixOf` pname then pname else "pet"

versionInfo :: String -> String
versionInfo name =
  unlines [ name ++ " - Personal Expense Tracker - version 0.2"
          , "Copyright (c) 2012-2013 - Fred Morcos <fred.morcos@gmail.com>"
          , "https://github.com/fredmorcos/pet.git" ]

usageHeader :: String -> String
usageHeader name = "Usage: " ++ name ++ " [ARGUMENTS] [FILES]"
