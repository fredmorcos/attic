module StringUtils where

import ListUtils
import Data.Char

quote :: String -> String
quote y = "`" ++ y ++ "'"

unlines' :: [String] -> String
unlines' []     = []
unlines' [x]    = x
unlines' (x:xs) = x ++ '\n' : unlines' xs

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

format :: String -> [String] -> String
format s xs = format' s $ zip [0..] xs
  where format' :: String -> [(Integer, String)] -> String
        format' s' []       = s'
        format' [] _        = []
        format' s' ((i, x'):xs') =
          format' (replaceAll ("{" ++ show i ++ "}") x' s') xs'
