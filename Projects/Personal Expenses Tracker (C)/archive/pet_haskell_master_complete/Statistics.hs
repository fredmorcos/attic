module Statistics where

import Expense
import Extras
import Errors
import Text.Printf
import Data.Time.Calendar
import Data.List

data StatMessage =
  TotalBetween { totalOf        :: Double,
                 firstDateOf    :: Day,
                 lastDateOf     :: Day,
                 numberOfDaysOf :: Integer } |
  AverageDay   { valueOf        :: Double  } |
  AverageDate  { valueOf        :: Double  } |
  AveragePay   { valueOf        :: Double  }

instance Show StatMessage where
  show (TotalBetween t f l n) =
    printf "A total of %.2f was spent between %s and %s (%d days)."
    t (show f) (show l) n
  show (AverageDay v)  = printf "An average of %.2f is spent per day." v
  show (AverageDate v) = printf "An average of %.2f is spent per date." v
  show (AveragePay v)  = printf "An average of %.2f is spent per payment." v

showStats :: [Expense] -> [Expense] -> String
showStats [] _     = show NoEntriesOriginal
showStats _  []    = show NoEntriesFiltered
showStats orig fil = unlines [
  show $ TotalBetween totalSum earliestDate latestDate (numDays orig),
  show $ AverageDay  (totalSum / (fromIntegral $ numDays orig)),
  show $ AverageDate (totalSum / (fromIntegral $ numDates orig)),
  show $ AveragePay  (totalSum / (fromIntegral $ length orig))]
  where totalSum     = sum $ map amountOf orig
        earliestDate = dateOf $ earliest orig
        latestDate   = dateOf $ latest orig

tagStats :: [Expense] -> String
tagStats l = let tagsList = allTags l
                 amountsList = map (\e -> totalAmountPerTag e l) tagsList
                 pairs = zip amountsList tagsList
                 sp = reverse $ sortBy (\a b -> compare (fst a) (fst b)) pairs
             in unlines $ map (\(a, t) -> printf "%10.2f %s" a t) sp

monthStats :: [Expense] -> String
monthStats l =
  let monthsList = allMonths l
      amountsList = map (\e -> totalAmountPerMonth e l) monthsList
      pairs = zip amountsList monthsList
      sp = reverse $ sortBy (\a b -> compare (fst a) (fst b)) pairs
  in unlines $ map (\(a, (m, y)) -> printf "%10.2f %d-%.2d (%9s %d)"
                                    a y m (showMonth m) y) sp
