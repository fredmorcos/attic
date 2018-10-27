module Extras where

import Numeric
import Data.List
import Data.Time.Calendar

type Month = Int
type Year  = Integer

monthNames :: [String]
monthNames = ["January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"]

showMonth :: Month -> String
showMonth = (monthNames !!)

(/==/) :: (Ord a, Eq a) => [a] -> [a] -> Bool
(/==/) x y = (sort x) == (sort y)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

splitWith :: Eq a => a -> [a] -> [[a]]
splitWith s l = __splitWith s l [] []
  where __splitWith _  []     acc r = r ++ [acc]
        __splitWith s2 (x:xs) acc r
          | x /= s2   = __splitWith s2 xs (acc ++ [x]) r
          | otherwise = __splitWith s2 xs [] (r ++ [acc])

earliest :: Ord a => [a] -> a
earliest l = (!! 0) $ sort l

latest :: Ord a => [a] -> a
latest l = (!! 0) $ reverse $ sort l

showReal :: Double -> String
showReal val = showFFloat (Just 2) val $ ""

average :: (Real a, Fractional b) => [a] -> b
average l = realToFrac (sum l) / genericLength l

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = if x `elem` xs then unique xs else (x:unique xs)

yearFrom :: Day -> Integer
yearFrom = yearFromGregorian . toGregorian

monthFrom :: Day -> Int
monthFrom = monthFromGregorian . toGregorian

yearFromGregorian :: (Integer, Int, Int) -> Integer
yearFromGregorian (y, _, _) = y

monthFromGregorian :: (Integer, Int, Int) -> Int
monthFromGregorian (_, m, _) = m
