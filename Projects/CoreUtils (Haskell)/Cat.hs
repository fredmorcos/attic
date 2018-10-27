{-# LANGUAGE CPP #-}

import           Control.Monad      ((>=>))
import           System.Environment (getArgs)

outFunc :: String -> String
#ifndef TAC
outFunc = id
#else
outFunc = unlines . reverse . lines
#endif

main :: IO ()
main = do args <- getArgs
          if not $ null args
            then mapM_ (readFile >=> (putStr . outFunc)) args
            else interact id
