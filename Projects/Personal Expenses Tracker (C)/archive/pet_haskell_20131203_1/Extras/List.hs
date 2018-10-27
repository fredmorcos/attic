module Extras.List where

import Data.List

-- anyIsPrefixOf :: Eq a => [[a]] -> [a] -> Bool
-- anyIsPrefixOf p t = any (== True) $ map (\v -> v `isPrefixOf` t) p

anyIsPrefixOf :: Eq a => [[a]] -> [a] -> Bool
anyIsPrefixOf p t = any (`isPrefixOf` t) p

anyIsEqualTo :: Eq a => [[a]] -> [a] -> Bool
anyIsEqualTo p t = any (== t) p
