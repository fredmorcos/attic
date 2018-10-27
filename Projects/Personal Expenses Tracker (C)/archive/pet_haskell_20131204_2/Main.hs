module Main where

import System.Environment
import Data.List
import Control.Monad
import System.IO
import System.IO.Error
import Data.Time

import Extras.List
import Extras.Maybe
import Extras.String

import System.Console.GetOpt
import Data.Maybe

data Flag = Verbose
          | Help
          | Version
          | Filename String
          | Output String
          deriving Show

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg  Verbose)         "Verbose output"
          , Option ['V'] ["version"] (NoArg  Version)         "Show version info"
          , Option ['h'] ["help"]    (NoArg  Help)            "Show help and usage info"
          , Option ['f'] ["file"]    (ReqArg Filename "FILE") "Input file, '-' means stdin"
          , Option ['o'] ["output"]  (ReqArg Output   "FILE") "Output file, '-' means stdout"
          ]

petOptions :: [String] -> IO ([Flag], [String])
petOptions args = case getOpt Permute options args of
  (o, n, [])   -> return (o, n)
  (_, _, errs) -> fail $ concat errs ++ usageInfo header options
  where header = "Usage: pet [OPTION...] files..."

-- verboseErr :: Options -> String -> IO ()
-- verboseErr = verboseGen stderr

-- verboseOut :: Options -> String -> IO ()
-- verboseOut = verboseGen stdout

-- verboseGen :: Handle -> Options -> String -> IO ()
-- verboseGen hdl opts str =
--   do t <- getZonedTime
--      when (verbose opts) $ hPutStrLn hdl $ stamp (show t) str

main :: IO ()
main = do args <- getArgs
          petOptions args >>= print
          -- putStrLn $ usageInfo "HEADER!" options
          -- case readOptions args of
          --   Left  x -> verboseErr (defaultOptions { verbose = True }) x
          --   Right y -> verboseErr y $ show y
