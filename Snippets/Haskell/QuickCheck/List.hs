module List where

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] [] = True
prefix [] _  = True
prefix _  [] = False
prefix sub@(s:sx) lst@(l:lx)
  | length sub > length lst = False
  | s == l = prefix sx lx
  | otherwise = False

-- replace :: [a] -> [a] -> [a] -> [a]
-- replace [] _ s = s
-- replace _ _ [] = []
-- replace old@(o:os) new s     -- old new str
--   | length old > length s = s
--   | 
