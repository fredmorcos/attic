pairToList :: (a, a) -> [a]
pairToList = uncurry (:) 

main :: IO ()
main = do print $ replicate 2 (5 :: Int) == pairToList (5, 5)
          print $ (\(f, s) -> [f, s]) (5 :: Int, 5 :: Int) ==
                  pairToList (5, 5)
