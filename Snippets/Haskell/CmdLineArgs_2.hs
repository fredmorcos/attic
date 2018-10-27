module Main where

import           System.Console.GetOpt
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optInput = input,
                optOutput = output } = opts
  input >>= output

data Options = Options  {
    optInput  :: IO String,
    optOutput :: String -> IO ()
  }

defaultOptions :: Options
defaultOptions = Options {
    optInput  = getContents,
    optOutput = putStr
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option "V" ["version"] (NoArg showVersion)         "show version number",
    Option "i" ["input"]   (ReqArg readInput "FILE")   "input file to read",
    Option "o" ["output"]  (ReqArg writeOutput "FILE") "output file to write"
  ]

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn "Commandline example 0.1"
  exitSuccess

readInput :: FilePath -> Options -> IO Options
readInput arg opt = return opt { optInput = readFile arg }

writeOutput :: FilePath -> Options -> IO Options
writeOutput arg opt = return opt { optOutput = writeFile arg }
