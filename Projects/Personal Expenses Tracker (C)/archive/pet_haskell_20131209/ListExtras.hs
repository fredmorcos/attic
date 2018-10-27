module ListExtras where

foldFilter :: (a -> b -> (a, Bool)) -> a -> [b] -> [b] -> (a, [b])
foldFilter _ acc []     lacc = (acc, lacc)
foldFilter f acc (x:xs) lacc =
  let (acc', filterOut) = f acc x
      lacc' = if filterOut then lacc else lacc ++ [x]
  in foldFilter f acc' xs lacc'
