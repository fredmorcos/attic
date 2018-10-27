module Expense where

import Extras
import Data.List
import Data.Time.Calendar

data Expense = Expense {
  amountOf :: Double,
  dateOf   :: Day,
  tagsOf   :: [String]}

instance Show Expense where
  show (Expense a d t) = unwords [showReal a, show d, intercalate "," t]

instance Eq Expense where
  (==) (Expense a1 d1 t1) (Expense a2 d2 t2) = a1 == a2 && d1 == d2 &&
    (sort $ unique t1) == (sort $ unique t2)

instance Ord Expense where
  compare e1 e2 = compare (dateOf e1) (dateOf e2)

showExpenseList :: [Expense] -> String
showExpenseList l = unlines $ map show l

numDays :: [Expense] -> Integer
numDays l | length l > 0 = (diffDays (dateOf lat) (dateOf ear)) + 1
          | otherwise    = 0
  where (ear, lat) = (earliest l, latest l)

numDates :: [Expense] -> Integer
numDates = genericLength . unique . map dateOf

yearOf :: Expense -> Year
yearOf = yearFrom . dateOf

monthOf :: Expense -> Month
monthOf = monthFrom . dateOf

filterByTag :: String -> [Expense] -> [Expense]
filterByTag t l = filter (\e -> t `elem` tagsOf e) l

filterByDate :: Day -> [Expense] -> [Expense]
filterByDate d l = filter (\e -> d == dateOf e) l

filterByMonth :: (Month, Year) -> [Expense] -> [Expense]
filterByMonth (m, y) l = filter (\e -> monthOf e == m && yearOf e == y) l

allTags :: [Expense] -> [String]
allTags l = unique $ foldl (\a e -> a ++ (tagsOf e)) [] l

totalAmountPerTag :: String -> [Expense] -> Double
totalAmountPerTag t l = sum $ map amountOf $ filterByTag t l

allMonths :: [Expense] -> [(Month, Year)]
allMonths l = unique $ map (\e -> (monthOf e, yearOf e)) l

totalAmountPerMonth :: (Month, Year) -> [Expense] -> Double
totalAmountPerMonth m l = sum $ map amountOf $ filterByMonth m l
