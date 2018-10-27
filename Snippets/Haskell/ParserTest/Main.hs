module Main where

import Control.Monad

data ParserState a = Running { line :: Int
                             , col :: Int
                             , result :: [String]
                             }
                   | Error { msg :: String
                           , running :: ParserState
                           }

instance Functor ParserState where
    fmap f

defaultRunning :: ParserState
defaultRunning = Running { line = 1, col = 1 }

defaultError :: ParserState
defaultError = Error { msg = "", running = defaultRunning }

test :: String
test = "None,person1,person2\nNone,shop1,shop2,shop3\ny 2012\nm 01\n"

main :: IO ()
main = putStrLn "foo"
