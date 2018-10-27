import           Control.Monad      (forever, liftM)
import           System.Environment (getArgs)

main :: IO ()
main = liftM unwords getArgs >>= forever . putStrLn
