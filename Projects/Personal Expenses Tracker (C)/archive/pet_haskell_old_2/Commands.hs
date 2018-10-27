module Commands where

import           Control.Monad
import           Expense

type Cmd = Expenses -> IO Expenses

execIOCmds :: Expenses -> [Cmd] -> IO Expenses
execIOCmds = foldM (\a f -> f a)

displayExpenses :: Expenses -> IO Expenses
displayExpenses exps = mapM_ print exps >> return exps

selectPerson :: String -> Expenses -> IO Expenses
selectPerson n exps = return $ filter (\e -> person e == n) exps
