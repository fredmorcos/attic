module Parser where

import Control.Applicative
import Control.Monad
import Data.Monoid

type Pred a = (a -> Bool, String)

newtype PosUpd a p = PosUpd (p -> a -> p)

instance Show (PosUpd a p) where
  show _ = "<pos update func>"

type ParserRes a p b = Either ([String], [String], p) (b, [a], [String], p, PosUpd a p)

newtype Parser a p b = Parser ([a] -> [String] -> p -> PosUpd a p -> ParserRes a p b)

runParser :: Parser a p b -> [a] -> [String] -> p -> PosUpd a p -> ParserRes a p b
runParser (Parser sp) = sp

instance Functor (Parser a p) where
  fmap f (Parser sp) = Parser $ \i w p u ->
    (\(x, r, w', p', _) -> (f x, r, w', p', u)) <$> sp i w p u

instance Applicative (Parser a p) where
  pure v = Parser $ \i w p u -> Right (v, i, w, p, u)
  (<*>) = ap

instance Alternative (Parser a p) where
  empty = Parser $ \_ w p _ -> Left (mempty, w, p)
  Parser p1 <|> Parser p2 = Parser $ \i w p u ->
    case p1 i w p u of Left _ -> p2 i w p u; x -> x

instance Monad (Parser a p) where
  fail = failList . (:[])
  return = pure
  f1 >>= f2 = Parser $ \i w p u ->
    runParser f1 i w p u >>= (\(x, r, w', p', _) -> runParser (f2 x) r w' p' u)

pos :: Parser a p p
pos = Parser $ \i w p u -> Right (p, i, w, p, u)

warn :: String -> Parser a p b
warn s = Parser $ \i w p u -> Right (, i, w ++ [s], p, u)

failListPos :: p -> [String] -> Parser a p b
failListPos p msgs = Parser $ \_ w _ _ -> Left (msgs, w, p)

failList :: [String] -> Parser a p b
failList msgs = Parser $ \_ w p _ -> Left (msgs, w, p)

-- Return the first item from the input without extracting it and
-- without doing any updates to positioning.
peek :: Parser a p a
peek = Parser $ \i@(x:_) w p u -> Right (x, i, w, p, u)

-- Return the first item from the input and update the positioning
-- accordingly using `u` from `Parser`.
item :: Parser a p a
item = Parser $ \(x:xs) w p u@(PosUpd uf) -> Right (x, xs, w, uf p x, u)

isEnd :: Eq a => Parser a p Bool
isEnd = Parser $ \i w p u -> return (i == mempty, i, w, p, u)

failEnd :: Eq a => String -> Parser a p ()
failEnd m = isEnd >>= \e -> when e $ failList $ "Unexpected end" : [m]

-- Parser satisfaction functions
sat1 :: Eq a => String -> Parser a p b -> Parser a p b
sat1 m p = failEnd m >> p

satMany :: Eq a => Parser a p b -> Parser a p [b]
satMany p = isEnd >>= \e ->
  if e then return [] else p >>= \r -> satMany p >>= \rs -> return $ r:rs

satMany1 :: Eq a => String -> Parser a p b -> Parser a p [b]
satMany1 m p = do c <- sat1 m p; r <- satMany p; return $ c:r

-- Predicate satisfaction functions
pSat1 :: Eq a => Pred a -> Parser a p a
pSat1 (p, d) =
  failEnd d >> peek >>= \c ->
  if p c then item else failList $ "Unsatisfied predicate" : [d]

pSatMany :: Eq a => Pred a -> Parser a p [a]
pSatMany pd@(p, _) =
  isEnd >>= \e -> if e then return [] else
    peek >>= \c -> if not $ p c then return [] else
      item >>= \i -> pSatMany pd >>= \r -> return $ i:r

pSatMany1 :: Eq a => Pred a -> Parser a p [a]
pSatMany1 pd = do c <- pSat1 pd; r <- pSatMany pd; return $ c:r
