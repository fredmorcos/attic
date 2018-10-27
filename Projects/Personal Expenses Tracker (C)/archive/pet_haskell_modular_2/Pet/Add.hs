module Pet.Add where

import Pet.Common.Errors
import Pet.Common.SHA1
import System.Directory
import System.IO

petAdd :: IO ()
petAdd = do
  dirExists <- doesDirectoryExist "_pet"
  if not dirExists
    then exitWith' notPetRepo
    else do putStr "Amount: " >> hFlush stdout
            amount <- getLine
            putStr "Date: " >> hFlush stdout
            date <- getLine
            putStr "Tags: " >> hFlush stdout
            tags <- getLine
            putStr "Note: " >> hFlush stdout
            note <- getLine
            dirExists' <- doesDirectoryExist "_pet"
            if not dirExists'
              then exitWith' notPetRepo
              else do sha1res <- sha1sum $ unlines [amount, date, tags, note]
                      case sha1res of
                        Right err -> exitWith' err
                        Left  out -> putStrLn out
