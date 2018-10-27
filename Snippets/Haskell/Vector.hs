module Vector where

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vector i j k) (Vector l m n) = (i * l) + (j * m) + (k * n)
