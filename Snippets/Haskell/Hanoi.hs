hanoiMinSteps :: Integer -> Integer
hanoiMinSteps 0 = 0
hanoiMinSteps n = 2 * (hanoiMinSteps (n - 1)) + 1

hanoiMinSteps' :: Integer -> Integer
hanoiMinSteps' 0 = 0
hanoiMinSteps' n = 1 + 2 * (hanoiMinSteps' (n - 1))

hanoiMinSteps'' :: Integer -> Integer
hanoiMinSteps'' n = (2 ^ n) - 1

hanoi :: ([Int], [Int], [Int]) -> ([Int], [Int], [Int])
hanoi ((a:as), b, c) = hanoi (as, c, b)

-- hanoi :: Int -> ([Int], [Int], [Int]) -> ([Int], [Int], [Int])
-- hanoi 1 (a, b, c) = if (length a) == 0 then
--                        (a, b, c)
--                     else
--                        (x, [y] ++ b, c) 
--                            where x = take i a
--                                  y = a !! i
--                                  i = (length a) - 1
-- hanoi n (a, b, c) = hanoi (n - 1) (c1, b1, a1) 
--                       where (a1, b1, c1) = hanoi 1 (a2, b2, c2) 
--                                              where (a2, b2, c2) = hanoi (n - 1) (a, c, b)

main :: IO ()
-- main = do putStrLn (show (hanoiMinSteps   1024))
--           putStrLn (show (hanoiMinSteps'  1024))
--           putStrLn (show (hanoiMinSteps'' 1024))
main = do putStrLn $ show $ hanoi ([1,2,3], [], [])
