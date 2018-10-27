-- | takeStrictlyLessThan take elements of a list whils their sum is
-- _strictly_ less than a given number
--
-- Point-free: I didnt' try without parameter, you can easily "hide" the 2nd
-- parameter (ie. takeStrictlyLessThan x = )
-- Level: MEDIUM
--
-- Examples:
-- >>> takeStrictlyLessThan (10::Int) [1..]
-- [1,2,3]
--
-- >>> takeStrictlyLessThan (3::Integer) $ repeat 1
-- [1,1]
--
-- >>> takeStrictlyLessThan (42::Int) $ []
-- []
--
-- takeStrictlyLessThan :: Choose your poison
takeStrictlyLessThan :: (Num a, Ord a) => a -> [a] -> [a]
takeStrictlyLessThan s l = concat $ drop ((length xs - 1) `max` 0) xs
  where xs = takeWhile (\x -> sum x < s) $ map (`take` l) [1..(length l + 1)]

main :: IO ()
main = do print $ takeStrictlyLessThan (10 :: Int) [1..]
          print $ takeStrictlyLessThan (3  :: Integer) $ repeat 1
          print $ takeStrictlyLessThan (42 :: Int) []
