import System.Environment

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn $ show $ calcRPN expr

calcRPN :: String -> Float
calcRPN e | length finalStack == 1 = head $ finalStack
          | otherwise              = error "Unbalanced expression"
  where finalStack = foldl evalCurrent [] $ words e
        evalCurrent acc next
          | next == "+"  = execOp2 (+)
          | next == "-"  = execOp2 (-)
          | next == "*"  = execOp2 (*)
          | next == "/"  = execOp2 (/)
          | next == "^"  = execOp2 (**)
          | next == "ln" = execOp1 (log)
          | next == "s"  = execOpA (sum)
          | otherwise   = (read next:acc)
          where execOp2 op = ((foldl1 op $ take 2 acc):(drop 2 acc))
                execOp1 op = ((op $ head $ take 1 acc):(drop 1 acc))
                execOpA op = [op acc]
