module Index where

import Expense

data MonthIndex = MonthIndex { month    :: Int
                             , expenses :: [Expense]
                             }

data YearIndex = YearIndex { year   :: Int
                           , months :: [MonthIndex]
                           }

data Index = Index { persons :: [String]
                   , shops   :: [String]
                   , index   :: [YearIndex]
                   }
