import Data.Char
import Control.Monad

main = forever $ do
  putStrLn "Give me some input:"
  input <- getLine
  putStrLn $ map toUpper input
