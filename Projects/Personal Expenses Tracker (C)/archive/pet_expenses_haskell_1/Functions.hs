module Functions (showExpenses) where

import Data.Time.Calendar (Day, toGregorian)
import Text.Printf (printf)
import Data.List (maximum)

showExpenses :: [(Day, Double, String)] -> String
showExpenses exp = unlines $ stringExpenses
  where amountMaxWidth = maximum $ map (\(d, a, c) -> length $ show a) exp
        stringExpenses = map (\e -> showExpenseElem e $ amountMaxWidth) exp

showExpenseElem :: (Day, Double, String) -> Int -> String
showExpenseElem (date, amount, category) amountMax =
  printf "%s  %*.2f  %-s" (show date) amountMax amount category
