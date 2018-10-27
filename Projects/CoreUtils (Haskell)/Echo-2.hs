import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= (putStrLn . unwords)
