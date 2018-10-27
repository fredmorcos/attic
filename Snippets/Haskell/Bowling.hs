module Bowling where

import Debug.Trace

nextTwoThrows :: [Int] -> Int
nextTwoThrows (x:y:_) = x + y
nextTwoThrows _ = error "NextTwoThrows: Not enough elements"

nextThrow :: [Int] -> Int
nextThrow (x:_) = x
nextThrow _ = error "NextThrow: Not enough elements"

score :: Int -> [Int] -> [Int]
score 0 _ = []
score _ [] = error "Something went wrong"
score rounds (10:xs) = 10 + nextTwoThrows xs : score (rounds - 1) xs
score rounds (x:y:xs)
  | x + y == 10 = 10 + nextThrow xs : score (rounds - 1) xs
  | otherwise = x + y : score (rounds - 1) xs
score rounds xs = error $ "Score: Unknown: " ++ show xs ++ " rounds = " ++ show rounds

-- return (ThrowScore, RoundScore, Result)
-- score :: [Int] -> (Int, Int, [Int])
-- score [] = (0, 0, [])
-- score [10] = (10, 10, [10])
-- score [x] = (x, x, [])
-- score (x:y:xs)
--   | x == 10     = let (_, roundScore, rest) = score (y:xs)
--                   in  (x, x + roundScore, (x + roundScore):rest)
--   | x + y == 10 = let (throwScore, _, rest) = score xs
--                   in  (x, x + y + throwScore, (x + y + throwScore):rest)
--   | otherwise   = let (_, _, rest) = score xs
--                   in  (x, x + y, (x + y):rest)

cumulativeScore :: Int -> [Int] -> [Int]
cumulativeScore _ [] = []
cumulativeScore y [x] = [y + x]
cumulativeScore y (x:xs) = x + y : cumulativeScore (x + y) xs

roundsScore :: Int -> [Int] -> [Int]
roundsScore rounds scores = trace ("rounds = " ++ show rounds) $
  let roundScores = score rounds scores
  in cumulativeScore 0 roundScores
