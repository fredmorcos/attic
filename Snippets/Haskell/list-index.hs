index :: [a] -> Int -> a
index (x:xs) 0 = x
index (x:xs) n = index xs (n - 1)
