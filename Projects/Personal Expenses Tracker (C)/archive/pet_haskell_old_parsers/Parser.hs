module Parser where

import Control.Applicative
import Control.Monad

data ParserErr a p = UnknownErr p
                   | UnexpectedEOF p
                   | BadItem a p

type Pred a = a -> Bool
type UpdPos a p = a -> p -> p

type ParserSucc a p b = ([a], p, UpdPos a p, b)
type ParserRes  a p b = Either (ParserErr a p) (ParserSucc a p b)

newtype Parser a p b = Parser ([a] -> p -> UpdPos a p -> ParserRes a p b)

parse :: Parser a p b -> [a] -> p -> UpdPos a p -> ParserRes a p b
parse (Parser par) = par

instance Functor (Parser a p) where
  f `fmap` par = Parser $ \i p u ->
    (\(i', p', u', o') -> (i', p', u', f o')) <$> parse par i p u

instance Applicative (Parser a p) where
  pure v = Parser $ \i p u -> Right (i, p, u, v)
  (<*>) = ap

instance Alternative (Parser a p) where
  empty = Parser $ \_ p _ -> Left $ UnknownErr p
  p1 <|> p2 = Parser $ \i p u -> case parse p1 i p u of Left _ -> parse p2 i p u

instance Monad (Parser a p) where
  return = pure
  p1 >>= f = Parser $ \i p u ->
    parse p1 i p u >>= (\(i', p', u', o') -> parse (f o') i' p' u')

pos :: Parser a p p
pos = Parser $ \i p u -> Right (i, p, u, p)

pfail :: ParserErr a p -> Parser a p b
pfail e = Parser $ \_ _ _ -> Left e

peek :: Parser a p a
peek = Parser $ \i@(x:_) p u -> Right (i, p, u, x)

next :: Parser a p a
next = Parser $ \(x:xs) p u -> Right (xs, u x p, u, x)

end :: Parser a p Bool
end = Parser $ \i p u -> Right (i, p, u, null i)

item :: Eq a => a -> Parser a p a
item c = Parser $ \i p u ->
  if null i then Left $ UnexpectedEOF p
  else let (x:xs) = i in if c == x then Right (xs, u x p, u, x)
                         else Left $ BadItem x p

satisfy :: Eq a => Pred a -> Parser a p a
satisfy f = peek >>= \c -> if f c then item c else pfail $ BadItem c p

-- munch' :: Parser a p [a] -> (a -> Bool) -> Parser a p [a]
-- munch' b f = isEnd >>= \e -> if e then b else peek >>= \c ->
--   if f c then munch' b f >>= \r -> return (c:r) else b

-- munch :: Pred a -> Parser a p [a]
-- munch = munch' $ return mempty

-- munch1 :: Pred a -> Parser a p [a]
-- munch1 = munch' empty
