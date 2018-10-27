f1 xs = map f xs where f x = x * 2 + 3
f1p xs = map (\x -> x * 2 + 3) xs

f2 xs = let f x y = read x + y in foldr f 1 xs
f2p xs = foldr (\x y -> read x + y) 1 xs
