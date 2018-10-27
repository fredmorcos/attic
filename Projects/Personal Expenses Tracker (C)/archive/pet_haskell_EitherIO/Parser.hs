module Parser where

-- TODO Somehow find a way to provide type constraints on data type
-- fields. This will avoid passing `Eq a` to every `Parser`
-- function. It will also help avoid passing `(p -> a -> p)` as an
-- "updatePositioning" function to every function that needs it by
-- creating a new "Positionable" type-class.

import           Control.Monad (when)
import           Data.Monoid   (mempty)

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

instance Monad (Parser a p) where
  return  v = Parser $ \i p -> Right (v, i, p)
  fail  msg = Parser $ \_ p -> Left  (msg,  p)
  f1 >>= f2 = Parser $ \i p -> runParser f1 i p >>=
                               (\(x, r, p') -> runParser (f2 x) r p')

-- Try the parser on the left, if it succeeds return its result, if it
-- fails return whatever (success or failure) comes back from the
-- parser on the right.
(<++) :: Parser a p b -> Parser a p b -> Parser a p b
Parser p1 <++ Parser p2 = Parser $ \i p ->
  case p1 i p of Left _ -> p2 i p
                 x      -> x

peek :: Parser a p a
peek = Parser $ \i@(x:_) p -> Right (x, i, p)

-- Return the first item from the input and update the positioning
-- accordingly using `f`.
item :: (p -> a -> p) -> Parser a p a
item f = Parser $ \(x:xs) p -> Right (x, xs, f p x)

isEnd :: Eq a => Parser a p Bool
isEnd = Parser $ \i p -> return (i == mempty, i, p)

failOnEnd :: Eq a => ErrorMessage -> Parser a p ()
failOnEnd msg = do e <- isEnd
                   when e $ fail $ "Unexpected end of input: " ++ msg

satisfy :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p a
satisfy f (p, d) = do
  failOnEnd d
  c <- peek
  if p c then item f else fail $ "Predicate not satisfied: " ++ d

satisfyMany :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p [a]
satisfyMany f pd@(p, _) = do
  e <- isEnd
  if e then return [] else do
    c <- peek
    if not $ p c then return [] else do
      i <- item f
      r <- satisfyMany f pd
      return $ i:r

satisfyMany1 :: Eq a => (p -> a -> p) -> Predicate a -> Parser a p [a]
satisfyMany1 f pd = do c <- satisfy f pd
                       r <- satisfyMany f pd
                       return $ c:r
