doublelist :: [Integer] -> [Integer]
doublelist []     = []
doublelist (x:xs) = (x * 2) : (doublelist xs)
