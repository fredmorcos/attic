module Extras where

splitWith :: Eq a => a -> [a] -> [[a]]
splitWith = splitWith' [] []
  where splitWith' lacc acc _ [] = reverse $ if not (null acc) then acc:lacc else lacc
        splitWith' lacc acc d (x:xs) = if x == d
                                       then if null acc
                                            then splitWith' lacc [] d xs
                                            else splitWith' (acc:lacc) [] d xs
                                       else splitWith' lacc (acc ++ [d]) d xs

