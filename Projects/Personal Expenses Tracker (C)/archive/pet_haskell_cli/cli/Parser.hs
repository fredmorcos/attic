module Parser where

import Control.Applicative

type Loc a l   = a -> l -> l
type Err   l   = ([String], l)
type Suc a l b = ([a], l, Loc a l, b)
type Ret a l b = Either (Err l) (Suc a l b)

type Pred a = a -> Bool

newtype Parser a l b = Parser ([a] -> l -> Loc a l -> Ret a l b)

parse :: Parser a l b -> [a] -> l -> Loc a l -> Ret a l b
parse (Parser p) = p

prettyParse :: Parser a l b -> [a] -> l -> Loc a l -> Either (Err l) ([a], l, b)
prettyParse (Parser p) i l u = case p i l u of
  Right (i_, l_, _, o) -> Right (i_, l_, o)
  Left (e_, l_) -> Left (e_, l_)

instance Functor (Parser a l) where
  fmap f (Parser p) = Parser $ \i l u -> case p i l u of
    Right (i_, l_, _, o) -> Right (i_, l_, u, f o)
    Left x -> Left x

instance Applicative (Parser a l) where
  pure v = Parser $ \i l u -> Right (i, l, u, v)
  (Parser p1) <*> p2 = Parser $ \i l u -> case p1 i l u of
    Right (i_, l_, _, f) -> parse (fmap f p2) i_ l_ u
    Left x -> Left x

instance Alternative (Parser a l) where
  empty = Parser $ \_ l _ -> Left (["<Alternative.empty>"], l)
  (Parser p1) <|> (Parser p2) = Parser $ \i l u -> case p1 i l u of
    Left (e_, _) -> case p2 i l u of
      Left (e__, _) -> Left (e__ ++ e_, l)
      Right x -> Right x
    Right x -> Right x

instance Monad (Parser a l) where
  return = pure
  (Parser p) >>= f = Parser $ \i l u -> case p i l u of
    Right (i_, l_, _, o) -> parse (f o) i_ l_ u
    Left x -> Left x

loc :: Parser a l l
loc = Parser $ \i l u -> Right (i, l, u, l)

err :: Maybe l -> String -> Parser a l b
err Nothing  e = Parser $ \_ l _ -> Left ([e], l)
err (Just l) e = Parser $ \_ _ _ -> Left ([e], l)

eof :: Parser a l Bool
eof = Parser $ \i l u -> Right (i, l, u, null i)

errEOF :: Bool -> String -> String
errEOF False e = unwords ["| Unexpected EOF, optionally expecting", e]
errEOF True  e = unwords ["| Unexpected EOF, expecting", e]

peek :: String -> Parser a l a
peek e = Parser $ \i l u ->
  if null i then Left ([errEOF False e], l)
  else let h = head i in Right (i, l, u, h)

satisfy :: Pred a -> String -> Parser a l a
satisfy p e = Parser $ \i l u ->
  if null i then Left ([errEOF True e], l)
  else let h = head i
       in if p h then Right (tail i, u h l, u, h)
          else Left ([unwords ["| Predicate not satisfied, expecting", e]], l)

item :: Parser a l a
item = satisfy (const True) "<Parser.item>"
