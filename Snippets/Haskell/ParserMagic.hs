module Parser where

data Parser a b = Parser ([a] -> Int -> Int -> Either (String, Int, Int) (b, [a], Int, Int))

-- newtype StringParser b = StringParser { stringParser :: String -> Int -> Int -> Either (String, Int, Int) (b, String, Int, Int) }

-- runStringParser :: StringParser b -> String -> Int -> Int -> Either (String, Int, Int) (b, String, Int, Int)
-- runStringParser (StringParser sp) = sp

-- instance Monad StringParser where
--   return v = StringParser $ \i l c -> Right (v, i, l, c)
--   fail msg = StringParser $ \_ l c -> Left  (msg,  l, c)
--   p >>= f  = StringParser $ \i l c -> runStringParser p i l c >>=
--                             (\(x, r, l', c') -> runStringParser (f x) r l' c')

runParser :: Parser a b -> [a] -> Int -> Int -> Either (String, Int, Int) (b, [a], Int, Int)
runParser (Parser sp) = sp

instance Monad (Parser a) where
  return v = Parser $ \i l c -> Right (v, i, l, c)
  fail msg = Parser $ \_ l c -> Left  (msg,  l, c)
  p >>= f  = Parser $ \i l c -> runParser p i l c >>=
                          (\(x, r, l', c') -> runParser (f x) r l' c')

peek :: Parser a a
peek = Parser $ \i@(x:_) l c -> Right (x, i, l, c)

-- item :: Parser a
-- item = Parser $ \(x:xs) l c -> Right (x, xs,
