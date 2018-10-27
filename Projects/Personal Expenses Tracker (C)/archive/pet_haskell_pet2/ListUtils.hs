module ListUtils where

import Data.List

split :: Eq a => a -> [a] -> [[a]]
split c ls = split' ls [] []
  where split' []     res cur = res ++ [cur]
        split' (x:xs) res cur =
          if x == c
          then split' xs (res ++ [cur]) []
          else split' xs res (cur ++ [x])

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = if x `elem` xs then unique xs else x:unique xs

replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll [] _ l = l
replaceAll _ _ [] = []
replaceAll old new l@(x:xs)
  | old `isPrefixOf` l = replaceAll old new $ new ++ drop (length old) l
  | otherwise          = x:replaceAll old new xs

lpad :: a -> [[a]] -> [[a]]
lpad p xs = map (\e -> replicate (longest - length e) p ++ e) xs
  where longest = maximum $ map length xs

rpad :: a -> [[a]] -> [[a]]
rpad p = map reverse . lpad p . map reverse
