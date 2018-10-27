data Foo = Foo {x :: Int, str :: String}
     deriving (Eq, Read, Show, Ord)

-- instance Eq Foo where
--   (==) (Foo x1 s1) (Foo x2 s2) = (x1 == x2) && (s1 == s2)
