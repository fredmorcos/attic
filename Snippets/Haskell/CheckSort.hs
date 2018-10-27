module Main where

checkSort :: Ord a => [a] -> Bool
checkSort [] = True
checkSort [_] = True
checkSort (x:y:xs) = (x <= y) && checkSort (y:xs)

checkSort' :: Ord a => [a] -> Bool
checkSort' = and . (drop 1 >>= zipWith (>=))

checkSort'' xs = and $ (drop 1 >>= zipWith (>=)) xs
checkSort''' xs = and $ zipWith (>=) (drop 1 xs) xs
checkSort'''' xs = 

main :: IO ()
main = do print $ map checkSort ([[1..10],
                                  [1,3,2],
                                  [],
                                  [4,3,2],
                                  [4,5,5]] :: [[Int]])
          print $ map checkSort' ([[1..10],
                                   [1,3,2],
                                   [],
                                   [4,3,2],
                                   [4,5,5]] :: [[Int]])
