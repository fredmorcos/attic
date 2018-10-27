module Extras where

import Data.Char

class Show a => PrettyShow a where
  prettyShow :: a -> String
  prettyShow x = show x

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = if x `elem` xs then unique xs else (x:unique xs)

toLowercase :: String -> String
toLowercase = map toLower

trimLeft :: String -> String
trimLeft [] = []
trimLeft xx@(x:xs)
  | isSpace x = trimLeft xs
  | otherwise = xx

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse

trim :: String -> String
trim = trimLeft . trimRight
