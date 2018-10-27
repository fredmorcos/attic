module Extras where

--import Data.Char
--import Data.List

--isSpaceOnly :: Char -> Bool
--isSpaceOnly c = isSpace c && c /= '\n'

isNewline :: Char -> Bool
isNewline c = c /= '\n'

--cleanSplitWith :: Eq a => a -> [a] -> [[a]]
--cleanSplitWith d t = filter (not . null) $ splitWith d t

--splitWith :: Eq a => a -> [a] -> [[a]]
--splitWith = splitWith' [] []
--  where splitWith' la a _ [] = reverse (a:la)
--        splitWith' la a d (x:xs) = if x == d then splitWith' (a:la) [] d xs
--                                   else splitWith' la (a ++ [x]) d xs

--(~~) :: Either a b -> (b -> Either a b) -> Either a b
--(~~) d f = case d of Left _ -> d; Right y -> f y

--trim :: String -> String
--trim = dropWhile isSpace . dropWhileEnd isSpace
