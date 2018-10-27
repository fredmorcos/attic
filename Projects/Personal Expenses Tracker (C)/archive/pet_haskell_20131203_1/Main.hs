module Main where

import System.Environment
import Data.List
import Control.Monad
import System.IO
import Data.Time

import Extras.List
import Extras.Maybe
import Extras.String

data OptionsHelp = None | Help | Version deriving (Show)

data Options = Options { filename :: Maybe String
                       , output   :: Maybe String
                       , verbose  :: Bool
                       , help     :: OptionsHelp
                       } deriving (Show)

defaultOptions :: Options
defaultOptions = Options { filename = Nothing
                         , output   = Nothing
                         , verbose  = False
                         , help     = None
                         }

readOption :: [String] -> String -> Either String String
readOption p v = case lastJust $ map (\x -> stripPrefix (x ++ "=") v) p of
  Nothing -> Left $ "Bad argument: " ++ v
  Just  x -> Right x

readComplexOption :: [String] -> String -> (String -> Options) ->
                     Either String Options
readComplexOption args val f = case readOption args val of
  Right x -> Right $ f x
  Left  y -> Left y

readOptions :: [String] -> Either String Options
readOptions args = foldM optionRead defaultOptions args
  where optionRead a v
          | verboseA  `anyIsEqualTo`  v = Right $ a { verbose = True }
          | helpA     `anyIsEqualTo`  v = Right $ a { help = Help }
          | versionA  `anyIsEqualTo`  v = Right $ a { help = Version }
          | filenameA `anyIsPrefixOf` v =
            readComplexOption filenameA v (\x -> a { filename = Just x })
          | outputA   `anyIsPrefixOf` v =
            readComplexOption outputA v (\x -> a { output = Just x })
          | otherwise = Left $ "Unknown argument: " ++ v
          where verboseA  = ["--verbose", "-v", "--v"]
                helpA     = ["--help", "-h", "--h"]
                versionA  = ["--version", "-V", "--V"]
                filenameA = ["--filename", "-f", "--f"]
                outputA   = ["--output", "-o", "--o"]

verboseErr :: Options -> String -> IO ()
verboseErr = verboseGen stderr

verboseOut :: Options -> String -> IO ()
verboseOut = verboseGen stdout

verboseGen :: Handle -> Options -> String -> IO ()
verboseGen hdl opts str =
  do t <- getZonedTime
     when (verbose opts) $ hPutStrLn hdl $ stamp (show t) str

main :: IO ()
main = do args <- getArgs
          case readOptions args of
            Left  x -> verboseErr (defaultOptions { verbose = True }) x
            Right y -> verboseErr y $ show y
