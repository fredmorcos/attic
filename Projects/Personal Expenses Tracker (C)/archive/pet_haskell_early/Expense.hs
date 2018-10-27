module Expense where

data Expense = Expense { amount :: Double
                       , day    :: Int
                       , person :: Int
                       , shop   :: Int
                       , tags   :: [String]
                       , note   :: String
                       } deriving Show
