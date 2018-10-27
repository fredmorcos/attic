module Pet.Expense where

import Extras.Data.List
import Data.List

data Expense = Expense { amountOf :: Double
                       , yearOf   :: Integer
                       , monthOf  :: Int
                       , dayOf    :: Int
                       , tagsOf   :: [String]
                       , noteOf   :: String
                       } deriving Show

instance Eq Expense where
  (==) (Expense a1 y1 m1 d1 t1 _) (Expense a2 y2 m2 d2 t2 _) =
    a1 == a2 && y1 == y2 && m1 == m2 && d1 == d2 &&
    (sort $ unique t1) == (sort $ unique t2)
