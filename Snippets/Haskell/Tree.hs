module Tree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node e left right)
  | x == e = tree
  | x  < e = Node e (treeInsert x left) right
  | x  > e = Node e left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node e left right)
  | x == e = True
  | x  < e = treeElem x left
  | x  > e = treeElem x right

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node e left right) = Node (f e) (fmap f left) (fmap f right)
