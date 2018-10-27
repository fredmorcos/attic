module SimpleParse.Parser where

import Control.Applicative
import Control.Monad
import Data.List

class    Positionable a    where vDelim :: a
instance Positionable Char where vDelim = '\n'
instance Positionable [a]  where vDelim = mempty

type Pred     a = a -> Bool                -- Predicate over a
type Pos        = (Int, Int)               -- Position in input
type PosUpd   a = Pos -> a -> Pos          -- Position update function
type ParserSt a = (Pos, [a])               -- Parser State

type ParserRes a b = Either (ParserSt a, String) (ParserSt a, b)
newtype Parser a b = Parser (PosUpd a -> ParserSt a -> ParserRes a b)

instance Functor (Parser a) where
  fmap f (Parser p) = Parser $ \u s -> fmap f <$> p u s

instance Applicative (Parser a) where
  pure v = Parser $ \_ s -> Right (s, v)
  (Parser p1) <*> (Parser p2) = Parser $ \u s -> case p1 u s of
    Right (s', f) -> fmap f <$> p2 u s'
    Left x -> Left x

instance Alternative (Parser a) where
  empty = Parser $ \_ s -> Left (s, mempty)
  (Parser p1) <|> (Parser p2) = Parser $ \u s -> p1 u s `eitherOr` p2 u s
    where (Left _) `eitherOr` e2 = e2
          e1       `eitherOr` _ = e1

instance Monad (Parser a) where
  return = pure
  (Parser p1) >>= f = Parser $ \u s ->
    p1 u s >>= \(s', v) -> let (Parser p2) = f v in p2 u s'

pfail :: String -> Parser a b
pfail msg = Parser $ \_ s -> Left (s, msg)

updatePos :: (Eq a, Positionable a) => PosUpd a
updatePos (l, c) x = if x == vDelim then (l + 1, 1) else (l, c + 1)

pos :: Parser a Pos
pos = Parser $ \_ s@(p, _) -> Right (s, p)

eof :: Parser a Bool
eof = Parser func where
  func _ s@(_, []) = Right (s, True)
  func _ s         = Right (s, False)

peek :: Parser a a
peek = Parser f where
  f _ s@(_, [])  = Left (s, "Unexpected end of file")
  f _ s@(_, x:_) = Right (s, x)

look :: Parser a [a]
look = Parser $ \_ s@(_, r) -> Right (s, r)

get :: Parser a a
get = Parser f where
  f _ s@(_, [])   = Left (s, "Unexpected end of file")
  f u   (p, x:xs) = Right ((u p x, xs), x)

item :: Eq a => a -> Parser a a
item i = peek >>= \x -> if i == x then get else pfail "Cannot consume item"

items :: Eq a => [a] -> Parser a [a]
items xs = look >>= \r -> if xs `isPrefixOf` r
                          then replicateM (length xs) get
                          else pfail "Cannot consume sequence"

satMany :: Parser a b -> Parser a [b]
satMany p = eof >>= \e -> if e then return mempty
                          else (do x <- p
                                   xs <- satMany p
                                   return $ x:xs) <|> return []

satMany1 :: Parser a b -> Parser a [b]
satMany1 p = p >>= \x -> satMany p >>= \xs -> return $ x:xs

psat :: Pred a -> Parser a a
psat p = peek >>= \x -> if p x then get else pfail "Cannot satisfy predicate"

psatMany :: Pred a -> Parser a [a]
psatMany p = satMany (psat p)

psatMany1 :: Pred a -> Parser a [a]
psatMany1 p = psat p >>= \x -> psatMany p >>= \xs -> return $ x:xs

psatN :: Int -> Pred a -> Parser a Int
psatN n p = liftM length (replicateM n $ psat p)

between :: Parser a b -> Parser a c -> Parser a d -> Parser a d
between open close p = open >> p >>= \x -> close >> return x

puntil :: Parser a b -> Parser a c -> Parser a [c]
puntil end p = (end >> return []) <|>
               (do x <- p
                   xs <- puntil end p
                   return $ x:xs)

-- sepBy :: Parser a b -> Parser a c -> Parser a [c]
-- sepBy (Parser sep) (Parser p) = Parser $ \u s -> case p u s of
--   Left (s', _) -> Right (s', [])
--   Right (s', x) -> case sep u s of
--     Left _ -> Right (s', [x])
--     Right (s'', _) -> do xs <- sepBy (Parser sep) (Parser p)
--                          return $ x:xs

  -- (do x <- (p <|> return [])
  --                 void sep
  --                 xs <- sepBy sep p
  --                 return $ x:xs) <|> return []

-- sepBy1 :: Parser a b -> Parser a c -> Parser a [c]
-- sepBy1 sep p = (do x <- p
--                    void sep
--                    xs <- sepBy sep p
--                    return (x:xs)) <|>
--                (p >>= \x -> return [x])
