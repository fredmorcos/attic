k = 1
--again, this won't work as expected
h :: Int -> Bool
h k = True
h _ = False

pred :: Int -> Int
pred (n+1) = n
