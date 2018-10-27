{-# LANGUAGE CPP #-}

import           Control.Monad      (when)
import           System.Environment (getArgs)

cropFunc :: String -> String
#ifndef TAIL
cropFunc = unlines . take 10 . lines
#else
cropFunc s = let ls = lines s in unlines $ drop (length ls - 10) ls
#endif

putData :: Bool -> [(String, String)] -> IO ()
putData _ [] = return ()
putData sn ((n, d):xs) = when sn (putStrLn $ "==> " ++ n ++ " <==") >>
                         if null xs then putStr (cropFunc d)
                         else putStrLn (cropFunc d) >> putData sn xs

main :: IO ()
main = do args <- getArgs
          datums <- mapM readFile args
          putData (length args > 1) $ zip args datums
