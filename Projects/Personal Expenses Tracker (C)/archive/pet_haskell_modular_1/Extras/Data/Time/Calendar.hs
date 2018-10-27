module Extras.Data.Time.Calendar where

import Data.Time.Calendar

dateValid :: Integer -> Int -> Int -> Bool
dateValid y m d = case fromGregorianValid y m d of
  Nothing -> False
  Just _  -> True
