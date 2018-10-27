import           Control.Monad      (liftM)
import           System.Environment (getArgs)
import           System.FilePath    (takeBaseName)

main :: IO ()
main = liftM (unlines . map baseName) getArgs >>= putStr
  where baseName n = let res = takeBaseName n in if null res then n else res
