module Parser where

import Control.Applicative
import Control.Monad

data ParserErrType a = UnexpectedEOI
                     | BadItem a
data ParserErr a p = ParserErr { errType :: ParserErrType a
                               , position :: p
                               }

type Pred a = a -> Bool
type PosUpd a p = a -> p -> p
type ParserSuc a p b = ([a], p, PosUpd a p, b)
type ParserRes a p b = Either (ParserErr a p) (ParserSuc a p b)
newtype Parser a p b = Parser ([a] -> p -> PosUpd a p -> ParserRes a p b)

parse :: Parser a p b -> [a] -> p -> PosUpd a p -> ParserRes a p b
parse (Parser par) = par

instance Functor (Parser a p) where
  f `fmap` par = Parser $ \i p u ->
    (\(i', p', u', o') -> (i', p', u', f o')) <$> parse par i p u

instance Applicative (Parser a p) where
  pure v = Parser $ \i p u -> Right (i, p, u, v)
  p1 <*> p2 = Parser $ \i p u -> case parse p1 i p u of
    Right (i', p', u', f') -> parse (fmap f' p2) i' p' u'

pos :: Parser a p p
pos = Parser $ \i p u -> Right (i, p, u, p)

pfail :: ParserErrType a -> Parser a p b
pfail e = Parser $ \_ p _ -> Left ParserErr { errType = e, position = p }

peek :: Parser a p a
peek = Parser $ \i@(x:_) p u -> Right (i, p, u, x)

next :: Parser a p a
next = Parser $ \(x:xs) p u -> Right (xs, u x p, u, x)

end :: Parser a p Bool
end = Parser $ \i p u -> Right (i, p, u, null i)

item :: Eq a => a -> Parser a p a
item c = Parser $ \i p u ->
  if null i then Left ParserErr { errType = UnexpectedEOI, position = p }
  else let h = head i; t = tail i
       in if c == h then Right (t, u h p, u, h)
          else Left ParserErr { errType = BadItem h, position = p }

-- satisfy :: Pred a -> Parser a p a
-- satisfy f = peek >>= \c -> if f c then item else empty

-- munch' :: Parser a p [a] -> (a -> Bool) -> Parser a p [a]
-- munch' b f = isEnd >>= \e -> if e then b else peek >>= \c ->
--   if f c then munch' b f >>= \r -> return (c:r) else b

-- munch :: Pred a -> Parser a p [a]
-- munch = munch' $ return mempty

-- munch1 :: Pred a -> Parser a p [a]
-- munch1 = munch' empty
