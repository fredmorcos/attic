-- Copyright (c) 2014, Fred Morcos
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- * Redistributions of source code must retain the above copyright
--   notice, this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the
--   distribution.
--
-- * Neither the name of the {organization} nor the names of its
--   contributors may be used to endorse or promote products derived
--   from this software without specific prior written permission.
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

-- TODO Somehow find a way to provide type constraints on data type
-- fields. This will avoid passing `Eq a` to every `Parser`
-- function. It will also help avoid passing `(p -> a -> p)` as an
-- "updatePositioning" function to every function that needs it. This
-- avoidance would be achieved by creating a new "Positionable"
-- typeclass. Currently this requires extensions that provide RankN
-- Types, Existential Quantification or Generalized Algebraic Data
-- Types. Also, this parser could possibly be implemented as an
-- Applicative only, but will lose all the do-notation sweetness that
-- GHC provides. In a future GHC version, and now that Applicative is
-- a parent typeclass of Monad, do-notation will possibly be used with
-- Applicatives as well and it will be nice to drop the Monad
-- instance.

import           Control.Applicative
import           Control.Monad
import           Data.Monoid

type ErrorMessage = String
type Description  = String
type Predicate a  = (a -> Bool, Description)

-- The result of a parser is either an error message of type
-- `ErrorMessage` (currently just a `String`) and the position `p`
-- where this error occurred, hence the tuple `(String, p)` or a
-- successful return of the expected result `b`, along with the
-- remainder of the input `[a]` and the current position `p`, hence
-- the triple `(b, [a], p)`.
type ParserRes a p b = Either (ErrorMessage, p) (b, [a], p)

-- A parser is a function from an input type `a` at a position `p` in
-- the input sequence, to either a successful result of type `b` or an
-- error. See ParserRes.
data Parser a p b = Parser ([a] -> p -> ParserRes a p b)

runParser :: Parser a p b -> [a] -> p -> ParserRes a p b
runParser (Parser sp) = sp

instance Functor (Parser a p) where
  fmap f (Parser sp) = Parser $ \i p ->
    (\(x, r, p') -> (f x, r, p')) <$> sp i p

instance Applicative (Parser a p) where
  pure v = Parser $ \i p -> Right (v, i, p)
  (<*>) = ap

instance Alternative (Parser a p) where
  empty = Parser $ \_ p -> Left (mempty, p)
  Parser p1 <|> Parser p2 = Parser $ \i p ->
    case p1 i p of Left _ -> p2 i p; x -> x

instance Monad (Parser a p) where
  return  v = Parser $ \i p -> Right (v, i, p)
  fail  msg = Parser $ \_ p -> Left  (msg,  p)
  f1 >>= f2 = Parser $ \i p ->
    runParser f1 i p >>= (\(x, r, p') -> runParser (f2 x) r p')

peek :: Parser a p a
peek = Parser $ \i@(x:_) p -> Right (x, i, p)

-- Return the first item from the input and update the positioning
-- accordingly using `f`.
item :: (p -> a -> p) -> Parser a p a
item f = Parser $ \(x:xs) p -> Right (x, xs, f p x)

isEnd :: Eq a => Parser a p Bool
isEnd = Parser $ \i p -> return (i == mempty, i, p)

failEnd :: Eq a => ErrorMessage -> Parser a p ()
failEnd msg = isEnd >>= \e -> when e $ fail $ "Unexpected end: " ++ msg

-- Parser satisfaction functions
sat1 :: Eq a => Parser a p b -> Parser a p b
sat1 p = p

satMany :: Eq a => Parser a p b -> Parser a p [b]
satMany p =
  isEnd >>= \e -> if e then return [] else
    p >>= \r -> satMany p >>= \rs -> return $ r:rs

satMany1 :: Eq a => Parser a p b -> Parser a p [b]
satMany1 p = do c <- sat1 p
                r <- satMany p
                return $ c:r

-- Predicate satisfaction functions
pSat1 :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p a
pSat1 f (p, d) =
  failEnd d >> peek >>= \c ->
  if p c then item f else fail $ "Unsatisfied predicate: " ++ d

pSatMany :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p [a]
pSatMany f pd@(p, _) =
  isEnd >>= \e -> if e then return [] else
    peek >>= \c -> if not $ p c then return [] else
      item f >>= \i -> pSatMany f pd >>= \r -> return $ i:r

pSatMany1 :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p [a]
pSatMany1 f pd = do c <- pSat1 f pd
                    r <- pSatMany f pd
                    return $ c:r
