module Extra.List where

pad,prepad :: a -> Int -> [a] -> [a]
pad c len l | length l >= len = l
            | otherwise = l ++ replicate (len - length l) c
prepad c len l | length l >= len = l
               | otherwise = replicate (len - length l) c ++ l
