module Expense where

import Extras
import Data.List
import Text.Printf

data Expense = Expense { amountOf :: Double
                       , dateOf   :: (Int, Int, Integer)
                       , tagsOf   :: [String]
                       , noteOf   :: String
                       } deriving (Show, Read)

instance Eq Expense where
  (==) (Expense a1 (d1, m1, y1) t1 _) (Expense a2 (d2, m2, y2) t2 _) =
    a1 == a2 && d1 == d2 && m1 == m2 && y1 == y2 &&
    (sort $ unique t1) == (sort $ unique t2)

instance Ord Expense where
  compare e1 e2 = compare (dateOf e1) (dateOf e2)

printExpense :: Expense -> String
printExpense (Expense a (d, m, y) t n) =
  printf "%.2f %d-%d-%d %s [%s]" a d m y (intercalate "," t) n

printExpenses :: [Expense] -> String
printExpenses es = unlines $ map printExpense es
