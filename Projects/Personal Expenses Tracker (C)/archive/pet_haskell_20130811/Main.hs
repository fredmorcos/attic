module Main where

import Parser
import Index
import Extras
import System.Environment

data ParseArgsErr = NoArgs | TooManyArgs

main :: IO ()
main = do args <- getArgs
          case parseArgs args of
            Left NoArgs -> putStrLn "no arguments given."
            Right filename -> do filedata <- readFile filename
                                 Right ParserState { psText  = filedata
                                                   , psLine  = 1
                                                   , psCol   = 1
                                                   , psTok   = ""
                                                   , psToks  = []
                                                   , psIndex = Index { persons = []
                                                                     , shops   = []
                                                                     , index   = []
                                                                     }
                                                   , psStage = PersonSection
                                                   }
                                 return ()

parseArgs :: [String] -> Either ParseArgsErr String
parseArgs []  = Left NoArgs
parseArgs [x] = Right x
parseArgs _   = Left TooManyArgs
