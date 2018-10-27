andd :: [Bool] -> Bool
andd []  = error "list empty"
andd [x] = x
andd (x:xs) = if x == True then
                andd xs
              else
                x

orr :: [Bool] -> Bool
orr [] = error "list empty"
orr [x] = x
orr (x:xs) = if x == True then
                True
             else
                orr xs
