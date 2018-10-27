module Parser where

import Data.Monoid (mempty)
import Control.Monad (MonadPlus, mzero, mplus)
import Control.Applicative (Applicative, pure,  (<*>),
                            Alternative, empty, (<|>))

type Pred      a     = a -> Bool
type PosUpd    a p   = a -> p -> p
type ParserErr   p   = (String, p)
type ParserSuc a p b = ([a], p, PosUpd a p, b)
type ParserRes a p b = Either (ParserErr p) (ParserSuc a p b)
newtype Parser a p b = Parser ([a] -> p -> PosUpd a p -> ParserRes a p b)

runParser :: Parser a p b -> [a] -> p -> PosUpd a p -> ParserRes a p b
runParser (Parser parser) = parser

instance Functor (Parser a p) where
  f `fmap` parser = Parser $ \i p u -> case runParser parser i p u of
    Right (i', p', _, o') -> Right (i', p', u, f o')
    Left  x               -> Left  x

instance Applicative (Parser a p) where
  pure v = Parser $ \i p u -> Right (i, p, u, v)
  p1 <*> p2 = Parser $ \i p u -> case runParser p1 i p u of
    Left  y               -> Left y
    Right (i', p', _, f') -> case runParser p2 i' p' u of
      Right (i'', p'', _, o'') -> Right (i'', p'', u, f' o'')
      Left  x                  -> Left  x

instance Alternative (Parser a p) where
  empty = Parser $ \_ p _ -> Left (mempty, p)
  p1 <|> p2 = Parser $ \i p u -> case runParser p1 i p u of
    Left  _ -> runParser p2 i p u
    Right x -> Right x

instance Monad (Parser a p) where
  return = pure
  p1 >>= f = Parser $ \i p u ->
    runParser p1 i p u >>= (\(i', p', _, o') -> runParser (f o') i' p' u)

instance MonadPlus (Parser a p) where
  mzero = empty
  mplus = (<|>)

pos :: Parser a p p
pos = Parser $ \i p u -> Right (i, p, u, p)

fail :: String -> Parser a p b
fail msg = Parser $ \_ p _ -> Left (msg, p)

peek :: Parser a p a
peek = Parser $ \i@(x:_) p u -> Right (i, p, u, x)

item :: Parser a p a
item = Parser $ \(x:xs) p u -> Right (xs, u x p, u, x)

isEnd :: Parser a p Bool
isEnd = Parser $ \i p u -> Right (i, p, u, null i)

satisfy :: Pred a -> Parser a p a
satisfy f = peek >>= \c -> if f c then item else empty

munch' :: Parser a p [a] -> (a -> Bool) -> Parser a p [a]
munch' b f = isEnd >>= \e -> if e then b else peek >>= \c ->
  if f c then munch' b f >>= \r -> return (c:r) else b

munch :: Pred a -> Parser a p [a]
munch = munch' $ return mempty

munch1 :: Pred a -> Parser a p [a]
munch1 = munch' empty
