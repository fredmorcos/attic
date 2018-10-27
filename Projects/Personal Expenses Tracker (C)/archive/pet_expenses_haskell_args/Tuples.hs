module Tuples where

fst :: (a, b, c) -> a
fst (x, _, _) = x

snd :: (a, b, c) -> b
snd (_, x, _) = x

thd :: (a, b, c) -> c
thd (_, _, x) = x
