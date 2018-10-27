rle :: String -> [(Int, Char)]
rle ""     = []
rle [x]    = [(1, x)]
rle (x:xs) = do
    let tmp = rle xs
    if (snd (tmp !! 0)) == x then
       ((((fst (tmp !! 0)) + 1), x) : (tail tmp))
    else
       ((1, x) : tmp)

rld :: [(Int, Char)] -> String
rld [] = ""
rld (x:xs) = if (fst x) == 0 then
                rld xs
             else
                val : (rld (((fst x) - 1, val) : xs))
                    where val = snd x
