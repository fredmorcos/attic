-- triangleArea :: Num a => a -> a -> a
triangleArea b h = (b * h) / 2

askSomething :: String -> IO Double
askSomething msg = do
                 putStrLn ("The " ++ msg ++ "?")
                 val <- getLine
                 return (read val::Double)

main = do
     height <- askSomething "height"
     base   <- askSomething "base"
     let area = triangleArea base height
     putStrLn ("The area of the triangle is " ++  (show area))
     return ()

