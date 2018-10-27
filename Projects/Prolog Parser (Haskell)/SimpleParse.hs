module SimpleParse where

import Control.Applicative
-- import Control.Monad
-- import Data.Monoid

-- A Parser from [a] to b
type ParserRes a b = Either ([a], String) ([a], b)
newtype Parser a b = Parser ([a] -> ParserRes a b)

-- | runParser runs  a `Parser` by unpacking it and  then executing it
-- over the remaining argument (the input) to runParser.
runParser :: Parser a b -> [a] -> ParserRes a b
runParser (Parser parser) = parser

-- | We  make our Parser  a Functor so we  can map functions  over the
-- (successful) result of executing the parser on some input.
instance Functor (Parser a) where
  -- |  We implement fmap by  unpacking the parser, executing  it over
  -- the input our new parser  will get and then applying the function
  -- `f`  on the  result in  case of  success. In  case of  failure we
  -- return the error as is.
  fmap f (Parser parser) = Parser $ \i -> case parser i of
    Left err -> Left err
    Right res -> Right $ fmap f res

instance Applicative (Parser a) where
  pure v = Parser $ \i -> Right (i, v)
  (Parser f) <*> (Parser parser) = Parser $ \i -> case f i of
    Left err -> Left err
    Right (remain', f') -> case parser remain' of
      Left err -> Left err
      Right res -> Right $ fmap f' res

instance Alternative (Parser a) where
  empty = Parser $ \i -> Left (i, [])
  (Parser p1) <|> (Parser p2) = Parser $ \i -> case p1 i of
    Right res -> Right res
    Left _ -> p2 i

instance Monad (Parser a) where
  return = pure
  (Parser parser) >>= f = Parser $ \i -> case parser i of
    Left err -> Left err
    Right (remain, res) -> case f res of
      (Parser fparser) -> fparser remain

peek :: Parser a a
peek = Parser func where
  func [] = Left ([], "Unexpected end of file")
  func i@(x:_) = Right (i, x)

item :: Parser a a
item = Parser func where
  func [] = Left ([], "Unexpected end of file")
  func (x:xs) = Right (xs, x)

eof :: Parser a Bool
eof = Parser func where
  func [] = Right ([], True)
  func i = Right (i, False)

sat :: Parser a b -> Parser a b
sat p = p

satMany :: Parser a b -> Parser a [b]
satMany (Parser p) = Parser $ \i -> case p i of
  Left _ -> Right (i, [])
  Right (remain, res) -> case runParser (satMany $ Parser p) remain of
    Left err -> Left err
    Right (remain', res') -> Right (remain', res:res')

satMany1 :: Parser a b -> Parser a [b]
satMany1 p = do
  x <- sat p
  xs <- satMany p
  return $ x:xs

pSat :: (a -> Bool) -> Parser a a
pSat predicate = do
  x <- peek
  if predicate x
    then item
    else Parser $ \i -> Left (i, "Could not satisfy predicate")

pSatMany :: (a -> Bool) -> Parser a [a]
pSatMany p = do
  end <- eof
  if end
    then return []
    else do
    x' <- peek
    if not $ p x'
      then return []
      else do x <- item
              xs <- pSatMany p
              return $ x:xs

pSatMany1 :: (a -> Bool) -> Parser a [a]
pSatMany1 p = do
  x <- pSat p
  xs <- pSatMany p
  return $ x:xs
