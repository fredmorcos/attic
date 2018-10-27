module Main where

import           Control.Monad      (when, (>=>))
import           System.Environment (getArgs, getProgName)

data HelpKeyword = CheckKeyword
                   deriving Show

-- Available/possible sub-commands
data Command = Help { keyword :: Maybe HelpKeyword }
             | Check { inputData :: String }
               deriving Show

-- This is the verbosity level in general for the application,
-- including sub-commands like help, add, remove and check
data VerboseLevel = Quiet | Verbose | Debug deriving Show

-- These are general options that apply to the top-level of the
-- application and to sub-commands as well
data Opts = Opts { verbose :: VerboseLevel
                 , command :: Command
                 } deriving Show

-- A command line parser is a function that takes a list of arguments
-- (strings) and returns either a) an error or b) a result and the
-- remainder of the arguments
newtype CLParser a = CLParser ([String] -> Either String (a, [String]))

runCLParser :: CLParser a -> [String] -> Either String (a, [String])
runCLParser (CLParser p) = p

instance Monad CLParser where
  return v = CLParser $ \i -> Right (v, i)
  fail msg = CLParser $ \_ -> Left  msg
  p >>= f  = CLParser $ runCLParser p >=> (\(x, r) -> runCLParser (f x) r)

(<++) :: Show a => CLParser a -> CLParser a -> CLParser a
CLParser p1 <++ CLParser p2 =
  CLParser $ \i -> case p1 i of Left  _ -> p2 i
                                Right x -> Right x

peek, item :: CLParser String
peek = CLParser $ \i@(x:_) -> Right (x, i)
item = CLParser $ \(x:xs)  -> Right (x, xs)

isEnd :: CLParser Bool
isEnd = CLParser $ \i -> return (null i, i)

failEnd :: String -> CLParser ()
failEnd msg = isEnd >>= \e -> when e $ fail $ "Unexpected end of input: " ++ msg

defaultOpts :: Opts
defaultOpts = Opts { verbose = Quiet
                   , command = Help { keyword = Nothing }
                   }

main :: IO ()
main = do args <- getArgs
          prog <- getProgName
          putStrLn $ "hello" ++ unwords (map show args) ++ "foo" ++ prog
