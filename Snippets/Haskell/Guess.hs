import System.Random

main :: IO ()
main = do g <- getStdGen
          let (randNum, newG) = randomR (1,10) g :: (Int, StdGen)
          putStrLn "Guess a number between 1 and 10: "
          guessFunc randNum

guessFunc :: Int -> IO ()
guessFunc randNum = do
  input <- getLine
  let res = compare randNum (read input :: Int)
  if res == GT
    then do putStrLn $ "It's greater than " ++ input ++ ", try again: "
            newStdGen
            guessFunc randNum
    else if res == LT
           then do putStrLn $ "It's less than " ++ input ++ ", try again: "
                   newStdGen
                   guessFunc randNum
           else putStrLn "Correct!"
