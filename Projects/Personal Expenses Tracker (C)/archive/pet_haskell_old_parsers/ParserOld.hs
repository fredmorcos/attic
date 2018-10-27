{-# LANGUAGE RankNTypes #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Monoid

newtype PosUpd a p = PosUpd (Monoid p => p -> a -> p)

instance Show (PosUpd a p) where
  show _ = "<pos-update-func>"

type ParserRes a p e w b =
  ( Monoid p
  , Monoid w
  ) =>
  Either
  ( p                           -- position
  , w                           -- warning(s)
  , e                           -- error
  )
  ( b                           -- output
  , [a]                         -- input
  , p                           -- position
  , w                           -- warning(s)
  , PosUpd a p                  -- position update function
  )

newtype Parser a p e w b = Parser (
  ( Monoid p                    -- position
  , Monoid w                    -- warning(s)
  ) =>
  [a]        ->                 -- input
  p          ->                 -- position
  w          ->                 -- warning(s)
  PosUpd a p ->                 -- position update function
  ParserRes a p e w b)          -- parser result

runParser :: Parser a p e w b -> [a] -> p -> w -> PosUpd a p
          -> ParserRes a p e w b
runParser (Parser parser) = parser

instance Functor (Parser a p e w) where
  f `fmap` parser = Parser $ \i p w u -> case runParser parser i p w u of
    Right (o', r', p', w', u') -> Right (f o', r', p', w', u'); Left x -> Left x

instance Applicative (Parser a p e w) where
  pure v = Parser $ \i p w u -> Right (v, i, p, w, u)
  -- p1 <*> p2 = Parser $ \i p w u -> case runParser p1 i p w u of
  --   Right (f', r', p', w', u') -> case runParser p2 r' p' w' u' of
  --     Right (o'', r'', p'', w'', u'') -> Right (f' o'', r'', p'', w'', u'')
  --     Left x -> Left x
  --   Left y -> Left y
  (<*>) = ap

instance Monad (Parser a p e w) where
  return = pure
  -- p1 >>= f = Parser $ \i p w u -> case runParser p1 i p w u of
  --   Right (o', r', p', w', u') -> runParser (f o') r' p' w' u'
  --   Left x -> Left x
  parser >>= f = Parser $ \i p w u ->
    runParser parser i p w u >>=
    (\(o', r', p', w', _) -> runParser (f o') r' p' w' u)

pos :: Parser a p e w p
pos = Parser $ \i p w u -> Right (p, i, p, w, u)

peek :: Parser a p e w a
peek = Parser $ \i p w u -> Right (head i, i, p, w, u)

item :: Parser a p e w a
item = Parser $ \(x:xs) p w u -> Right (x, xs, p, w, u)

end :: Parser a p e w Bool
end = Parser $ \i p w u -> Right (null i, i, p, w, u)

pFail :: e -> Parser a p e w b
pFail e = Parser $ \_ p w _ -> Left (p, w, e)

wconcat :: w -> Parser a p e w ()
wconcat x = Parser $ \i p w u -> Right ((), i, p, w `mappend` x, u)
