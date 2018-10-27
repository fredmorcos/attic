zipp :: [a] -> [b] -> [(a, b)]
zipp (x:xs) (y:[]) = [(x, y)]
zipp (x:[]) (y:ys) = [(x, y)]
zipp (x:xs) (y:ys) = ((x, y):(zip xs ys))
