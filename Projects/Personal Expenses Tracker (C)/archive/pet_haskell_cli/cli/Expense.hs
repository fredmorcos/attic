module Expense where

data Expense = Expense { amount :: Double
                       , date :: (Int, Int, Int)
                       , person :: String
                       , shop :: String
                       , tags :: [String]
                       , note :: Maybe String
                       } deriving Show
