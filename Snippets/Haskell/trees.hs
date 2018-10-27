data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap, treeMap2 :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch x y) = Branch (treeMap f x) (treeMap f y)

treeMap2 f = g where
         g (Leaf x) = Leaf (f x)
         g (Branch x y) = Branch (g x) (g y)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fb fl = g where
         g (Leaf x) = fl x
         g (Branch l r) = fb (g l) (g r)

tree1 :: Tree Integer
tree1 = 
    Branch
       (Branch 
           (Branch 
               (Leaf 1) 
               (Branch (Leaf 2) (Leaf 3))) 
           (Branch 
               (Leaf 4) 
               (Branch (Leaf 5) (Leaf 6)))) 
       (Branch
           (Branch (Leaf 7) (Leaf 8)) 
           (Leaf 9))

doubleTree = treeMap (* 2)
sumTree = treeFold (+) id
fringeTree = treeFold (++) (: [])

unfoldTree :: (b -> Either (b, b) a) -> b -> Tree a
unfoldTree f x = case (f x) of
                      Right a      -> Leaf a
                      Left  (l,r)  -> Branch (unfoldTree f l, unfoldTree f r)

