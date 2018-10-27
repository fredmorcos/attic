module Expense where

import           Data.List

data Expense = Expense { amount :: Double
                       , person :: String
                       , shop   :: String
                       , date   :: (Int, Int, Int)
                       , tags   :: [String]
                       , note   :: String
                       } deriving Show

type Expenses = [Expense]

instance Eq Expense where
  (Expense a1 p1 s1 d1 t1 _) == (Expense a2 p2 s2 d2 t2 _) =
    a1 == a2 && p1 == p2 && s1 == s2 && d1 == d2 && sort t1 == sort t2

instance Ord Expense where
  compare (Expense a1 p1 s1 d1 _ _) (Expense a2 p2 s2 d2 _ _) =
    if null equals then EQ else head equals
    where equals = filter (/= EQ) [compare d1 d2, compare a1 a2,
                                   compare p1 p2, compare s1 s2]
