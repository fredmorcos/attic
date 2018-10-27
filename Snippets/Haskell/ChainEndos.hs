module Main where

import           Data.Monoid

chainEndos :: [a -> a] -> a -> a
-- chainEndos = foldl (.) id
-- mconcat :: [a] -> a
--            [a -> a] -> (a -> a)
-- mempty  :: a
--            a -> a
-- mappend :: a -> a -> a
-- appEndo :: Endo a -> a -> a
--            Endo (a -> a) -> a -> a
chainEndos = appEndo . Endo . mconcat

main :: IO ()
main = do print $ chainEndos [('h':),('e':)] "llo"
          print $ chainEndos [(+ 1),(* 3)] (2 :: Int)
          print $ chainEndos [] (12 :: Int)
