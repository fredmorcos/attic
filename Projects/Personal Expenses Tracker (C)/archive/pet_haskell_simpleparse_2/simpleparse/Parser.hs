{-# LANGUAGE RankNTypes #-}

-- Copyright (c) 2014, Fred Morcos
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- * Redistributions of source code must retain the above copyright
-- notice, this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the
-- distribution.
--
-- * Neither the name of Fred Morcos nor the names of its contributors
-- may be used to endorse or promote products derived from this
-- software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
-- INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.

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
