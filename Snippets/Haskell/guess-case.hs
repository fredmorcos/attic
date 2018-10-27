doguess num = do
        putStrLn "enter your guess"
        guess <- getLine
        case compare (read guess) num of
             LT -> do putStrLn "too low"
                      doguess num
             GT -> do putStrLn "too hight"
                      doguess num
             EQ -> do putStrLn "you win"
