data Section = Section {getA :: Int,
                        getB :: Int,
                        getC :: Int} deriving Show
type RoadSystem = [Section]

-- instance Read RoadSystem where
--   read str =  $ lines str

main :: IO ()
main = do contents <- readFile "heathrowtolondon.txt"
          return ()

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = [[]]
chunkify 0 xs = [xs]
chunkify n xs | n >= length xs = [take n xs]
              | otherwise      = (take n xs : chunkify n (drop n xs))
