import           Control.Monad      (liftM)
import           System.Environment (getExecutablePath)
import           System.FilePath    (takeDirectory)

main :: IO ()
main = liftM takeDirectory getExecutablePath >>= putStrLn
