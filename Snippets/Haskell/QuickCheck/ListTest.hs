module Main where

import List
import Test.QuickCheck

prop_ownprefix :: Eq a => [a] -> Int -> Property
prop_ownprefix xs n = n > 0 ==> ((take n xs) `prefix` xs) == True

prop_selfprefix :: Eq a => [a] -> Bool
prop_selfprefix xs = (xs `prefix` xs) == True

prop_emptysub :: Eq a => [a] -> [a] -> Property
prop_emptysub sub xs = null sub ==> ((sub `prefix` xs) == True)

prop_bigsub :: Eq a => [a] -> [a] -> Property
prop_bigsub sub xs = length sub > length xs ==> ((sub `prefix` xs) == False)

prop_emptylist :: Eq a => [a] -> [a] -> Property
prop_emptylist sub xs = not (null sub) ==> null xs ==> ((sub `prefix` xs) == False)

prop_stress :: Eq a => [a] -> [a] -> Bool
prop_stress sub xs = ((take (length sub) xs) == sub) == (sub `prefix` xs)

args :: Args
args = stdArgs
  { maxSuccess = 1000
  , maxDiscardRatio = 20
  , chatty = True
  }

main :: IO ()
main = do
  quickCheckWith args (prop_selfprefix :: [Int] -> Bool)
  quickCheckWith args (prop_ownprefix  :: [Int] -> Int -> Property)
  quickCheckWith args (prop_emptysub   :: [Int] -> [Int] -> Property)
  quickCheckWith args (prop_bigsub     :: [Int] -> [Int] -> Property)
  quickCheckWith args (prop_emptylist  :: [Int] -> [Int] -> Property)
  quickCheckWith args (prop_stress     :: [Int] -> [Int] -> Bool)
