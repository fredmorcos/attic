qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = let smaller = qsort' [a | a <- xs, a <= x]
                    larger  = qsort' [a | a <- xs, a >  x]
                in  smaller ++ [x] ++ larger
