fac :: Integer -> Integer
fac 0 = 1
fac n = n * (fac (n - 1))

facList :: Integer -> [Integer]
facList n = map fac [0..n]

isEven :: Int -> Bool
isEven n = ((mod n 2) == 0)

retainEven :: [Int] -> [Int]
retainEven l = filter isEven l

retainEvenTuple, retainEvenTupleBetter :: [(Int, Int)] -> [Int]
retainEvenTuple l  = [fst n | n <- l, isEven (snd n)]
retainEvenTupleBetter l = [x | (x,y) <- l, isEven y]

returnDiv :: Int -> [Int] -> [Int]
returnDiv 0 _ = error "div by zero"
returnDiv n l = [x | x <- l, (mod x n) == 0]

listTails :: [[Int]] -> [[Int]]
listTails l = [tail x | x <- l, (length x) > 0, (head x) > 5]
