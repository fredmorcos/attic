module Index where

import Expense

data MonthIndex = MonthIndex { month    :: Int
                             , expenses :: [Expense]
                             } deriving Show

data YearIndex = YearIndex { year   :: Int
                           , months :: [MonthIndex]
                           } deriving Show

data Index = Index { persons :: [String]
                   , shops   :: [String]
                   , index   :: [YearIndex]
                   } deriving Show
