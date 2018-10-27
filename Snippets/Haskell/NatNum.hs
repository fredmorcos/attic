sumNatNums :: Integer -> Integer
sumNatNums 1 = 1
sumNatNums n = n + sumNatNums (n - 1)

sumNatNums' :: Integer -> Integer -> Integer
sumNatNums' 1 acc = acc + 1
sumNatNums' n acc = sumNatNums' (n - 1) (acc + n)

main = do putStrLn $ show $ sumNatNums 5000000
          putStrLn $ show $ sumNatNums' 5000000 0
