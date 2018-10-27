module Expense where

data Expense = Expense { amount :: Int
                       , day    :: Int
                       , person :: Int
                       , shop   :: Int
                       , tags   :: [String]
                       , note   :: String
                       } deriving Show
