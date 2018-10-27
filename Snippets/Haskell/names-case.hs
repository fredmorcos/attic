fun = do putStrLn "haskell is fun"

main = do
     putStrLn "enter your name"
     name <- getLine
     case name of
          "Simon" -> fun
          "John"  -> fun
          "Phil"  -> fun
          "Koen"  -> do putStrLn "debugging haskell"
          _       -> do putStrLn "i dont know you"
