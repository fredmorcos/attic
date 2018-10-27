import System.IO
import System.Environment
import Control.Monad
import Data.Time
import Data.List
import Data.Maybe

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

updateOptions :: Options -> [String] -> Options
updateOptions opts args = foldM optionRead defaultOptions args
  where optionRead a v
          | v == "--verbose" || v == "-v" = Right $ a { verbose = True }
          | v == "--help"    || v == "-h" = Right $ a { help = Help }
          | v == "--version" || v == "-V" = Right $ a { help = Version }
          | filenameP || fP = if any isJust results
                              then Right $ a { filename = Just fname }
                              else Left $ "Bad argument: " ++ v
          where filenameP = "--filename" `isPrefixOf` v
                fP = "-f" `isPrefixOf` v
                results = [stripPrefix "--filename=" v, stripPrefix "--f=" v]
                fname = foldl (\a' v' ->
                                case v' of Nothing -> a'
                                           (Just x) -> x) "" results

-- updateOptions :: Options -> [String] -> Options
-- updateOptions opts [] = opts
-- updateOptions opts (x:xs) =
-- updateOptions opts [x]
--   | x == "--verbose" || x == "-v" = opts { verbose = True }
--   | x == "--help"    || x == "-h" = opts { help = Help }
--   | x == "--version" || x == "-V" = opts { help = Version }
--   | otherwise = opts
-- updateOptions opts (x:y:xs)
--   | x == "--filename" || x == "-f" =
--     updateOptions (opts { filename = Just y }) xs
--   | x == "--output"   || x == "-o" =
--     updateOptions (opts { output = Just y }) xs
--   | otherwise = updateOptions opts (y:xs)

bracketize :: String -> String
bracketize s = "[" ++ s ++ "]"

stamp :: String -> String -> String
stamp stp msg = bracketize stp ++ " " ++ msg

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
          let opts = updateOptions defaultOptions args
          verboseErr opts $ show opts
