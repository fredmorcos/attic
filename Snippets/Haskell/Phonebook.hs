phoneBook :: [(String, String)]
phoneBook = [("fred", "123"), ("foo", "345"), ("bar", "567")]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k, v):xs) = if k == key
                             then Just v
                          else
                             findKey key xs

findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing 
