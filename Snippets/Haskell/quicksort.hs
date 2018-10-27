quickSort []     = []
quickSort [x]    = [x]
quickSort (x:xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
                   where less  = filter (< x)  xs
                         equal = filter (== x) xs
                         more  = filter (> x)  xs
