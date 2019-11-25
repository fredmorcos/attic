module Main where

-- isPrime :: Integer -> Bool
-- isPrime n = 
--  if (length [x | x <- [2..(div n 2)], (mod n x) == 0]) == 0 then
--     True
--  else
--     False

-- largestPrime :: Integer -> Integer
-- largestPrime n =
--  [x | x <- [(div n 2)..2], isPrime x, (mod n x) == 0] !! 0

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper _ 1 = True
isPrimeHelper n x = if (mod n x) == 0 then
                       False
                    else
                       isPrimeHelper n (x - 1)

isPrime :: Integer -> Bool
isPrime n = isPrimeHelper n (toInteger (floor (sqrt (fromIntegral n))))

largestPrimeHelper :: Integer -> Integer -> Integer
largestPrimeHelper _ 1 = 1
largestPrimeHelper n x = if (((mod n x) == 0) && (isPrime x)) then
                            x
                         else
                            largestPrimeHelper n (x - 1)

largestPrime :: Integer -> Integer
largestPrime n =
 largestPrimeHelper n (toInteger (floor (sqrt (fromIntegral n))))
