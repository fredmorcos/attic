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

import Control.Applicative
import Control.Monad
import Data.Monoid

type Predicate a = (a -> Bool, String)

-- The result of a parser is either an error message of type
-- `ErrorMessage` (currently just a `String`) and the position `p`
-- where this error occurred, hence the tuple `(String, p)` or a
-- successful return of the expected result `b`, along with the
-- remainder of the input `[a]` and the current position `p`, hence
-- the triple `(b, [a], p)`.
type ParserRes a p b = Either ([String], p) (b, [a], p)

-- A parser is a function from an input type `a` at a position `p` in
-- the input sequence, to either a successful result of type `b` or an
-- error. See ParserRes.
newtype Parser a p b = Parser ([a] -> p -> ParserRes a p b)

runParser :: Parser a p b -> [a] -> p -> ParserRes a p b
runParser (Parser sp) = sp

instance Functor (Parser a p) where
  f `fmap` (Parser parser) = Parser $ \i p -> case parser i p of
    Left  err1               -> Left err1
    Right (val1, rem1, pos1) -> Right (f val1, rem1, pos1)

instance Applicative (Parser a p) where
  pure v = Parser $ \i p -> Right (v, i, p)
  (Parser parser1) <*> (Parser parser2) = Parser $ \input pos ->
    case parser1 input pos of
      Left  err1                 -> Left  err1
      Right (func1, rem1, pos1)  -> case  parser2 rem1 pos1 of
        Left  (msg2, _)          -> Left  (msg2, pos1)
        Right (val2, rem2, pos2) -> Right (func1 val2, rem2, pos2)

instance Alternative (Parser a p) where
  empty = Parser $ \_ p -> Left (mempty, p)
  Parser parser1 <|> Parser parser2 = Parser $ \i p ->
    case parser1 i p of
      Right y           -> Right y
      Left  (err1, _)   -> case parser2 i p of
        Right x         -> Right x
        Left  (err2, _) -> Left (err1 `mappend` err2, p)

instance Monad (Parser a p) where
  fail = failList . (:[])
  return v = Parser $ \i p -> Right (v, i, p)
  f1 >>= f2 = Parser $ \i p ->
    runParser f1 i p >>= (\(x, r, p') -> runParser (f2 x) r p')

failList :: [String] -> Parser a p b
failList msgs = Parser $ \_ p -> Left (msgs, p)

peek :: Parser a p a
peek = Parser $ \i@(x:_) p -> Right (x, i, p)

-- Return the first item from the input and update the positioning
-- accordingly using `f`.
item :: (p -> a -> p) -> Parser a p a
item f = Parser $ \(x:xs) p -> Right (x, xs, f p x)

isEnd :: Eq a => Parser a p Bool
isEnd = Parser $ \i p -> return (i == mempty, i, p)

failEnd :: Eq a => String -> Parser a p ()
failEnd m = isEnd >>= \e -> when e $ failList $ "Unexpected end" : [m]

-- Parser satisfaction functions
sat1 :: Eq a => String -> Parser a p b -> Parser a p b
sat1 m p = failEnd m >> p

satMany :: Eq a => Parser a p b -> Parser a p [b]
satMany p =
  isEnd >>= \e -> if e then return [] else
    p >>= \r -> satMany p >>= \rs -> return $ r:rs

satMany1 :: Eq a => String -> Parser a p b -> Parser a p [b]
satMany1 m p = do c <- sat1 m p; r <- satMany p; return $ c:r

-- Predicate satisfaction functions
pSat1 :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p a
pSat1 f (p, d) =
  failEnd d >> peek >>= \c ->
  if p c then item f else failList $ "Unsatisfied predicate" : [d]

pSatMany :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p [a]
pSatMany f pd@(p, _) =
  isEnd >>= \e -> if e then return [] else
    peek >>= \c -> if not $ p c then return [] else
      item f >>= \i -> pSatMany f pd >>= \r -> return $ i:r

pSatMany1 :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p [a]
pSatMany1 f pd = do c <- pSat1 f pd; r <- pSatMany f pd; return $ c:r
