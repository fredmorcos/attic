doguess num = do
        putStrLn "enter your guess:"
        guess <- getLine
        if (read guess) < num
           then do putStrLn "too low"
                   doguess num
           else if (read guess) > num
                   then do putStrLn "too high"
                           doguess num
                   else do putStrLn "you win"
