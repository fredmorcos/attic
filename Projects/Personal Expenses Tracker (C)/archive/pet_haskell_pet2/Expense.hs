module Expense where

import ListUtils
import StringUtils
import Pretty
import Data.List

data Expense = Expense { amountOf :: Double
                       , dateOf   :: (Integer, Int, Int)
                       , tagsOf   :: [String]
                       , noteOf   :: String
                       } deriving (Show, Read)

instance Eq Expense where
  (==) (Expense a1 (d1, m1, y1) t1 _) (Expense a2 (d2, m2, y2) t2 _) =
    a1 == a2 && d1 == d2 && m1 == m2 && y1 == y2 &&
    sort (unique t1) == sort (unique t2)

instance Ord Expense where
  compare e1 e2
    | y1 /= y2  = compare y1 y2
    | m1 /= m2  = compare m1 m2
    | d1 /= d2  = compare d1 d2
    | otherwise = EQ
    where (d1, m1, y1) = dateOf e1
          (d2, m2, y2) = dateOf e2

instance PrettyShow Expense where
  pShow (Expense a (d, m, y) t n) =
    format "{0}  {1}-{2}-{3}  {4}  -> {5}"
    [show a, show d, show m, show y, intercalate "," t, n]
