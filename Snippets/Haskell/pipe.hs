import Control.Monad

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do putStrLn $ reverseSentence line
--             main

main = do
  line <- getLine
  when (not $ null line) $ do
    putStrLn $ reverseSentence line
    main

reverseSentence :: String -> String
-- reverseSentence s = unwords (map reverse (words s))
reverseSentence = unwords . map reverse . words

-- reverseSentence :: String -> String
-- reverseSentence s = unwords (map reverseWord (words s))

-- reverseWord :: String -> String
-- reverseWord word = reverseWordHelper "" word

-- reverseWordHelper :: String -> String -> String
-- reverseWordHelper res [] = res
-- reverseWordHelper res (x:xs) = reverseWordHelper (x:res) xs 
