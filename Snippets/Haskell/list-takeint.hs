takeint :: Int -> [a] -> [a]
takeint _ []     = []
takeint 0 _      = []
takeint n (x:xs) = x : (takeint (n - 1) xs)

dropint :: Int -> [a] -> [a]
dropint _ [] = []
dropint 0 l  = l
dropint n (x:xs) = dropint (n - 1) xs

sumint :: [Int] -> Int
sumint [] = 0
sumint (x:xs) = x + (sumint xs)

scansum :: [Int] -> [Int]
scansum l = scansum_ l 0
scansum_ :: [Int] -> Int -> [Int]
scansum_ [] _ = []
scansum_ (x:xs) n = c:(scansum_ xs c)
         where c = x + n

listdiff :: [Int] -> [Int] -> [Int]
listdiff [] _  = []
listdiff _  [] = []
listdiff (x:xs) (y:ys) = (y - x):(listdiff xs ys)

diffs :: [Int] -> [Int]
diffs [] = []
diffs [x] = []
diffs (x:xs) = listdiff (listhead (x:xs)) xs

listhead :: [a] -> [a]
listhead [] = []
listhead [x] = []
listhead (x:xs) = x:(listhead xs)

index :: [a] -> Int -> a
index [] _ = error "Empty list"
index (x:xs) 0 = x
index (x:xs) n = index xs (n - 1)

indexht :: [a] -> Int -> a
indexht [] _ = error "Empty list"
indexht l  0 = head l
indexht l  n = indexht (tail l) (n - 1)

factors :: Int -> [Int]
factors p = [f | f <- [1..p], p `mod` f == 0]

listfactors :: [Int] -> [[Int]]
listfactors [] = []
listfactors (x:xs) = (factors x):(listfactors xs)
