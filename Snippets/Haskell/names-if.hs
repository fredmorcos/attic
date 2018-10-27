main = do
     putStrLn "enter your name"
     name <- getLine
     if name == "Simon" || name == "John" || name == "Phil" then
        do putStrLn "haskell is fun"
     else if name == "Koen" then
        do putStrLn "debugging haskell is fun"
     else
        do putStrLn "i dont know who you are"
