module Extra.Tuple where

fst3 :: (a, b, c) -> a
fst3 (v, _, _) = v

snd3 :: (a, b, c) -> b
snd3 (_, v, _) = v

trd3 :: (a, b, c) -> c
trd3 (_, _, v) = v
