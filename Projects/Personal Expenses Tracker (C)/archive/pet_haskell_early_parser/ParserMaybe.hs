module Parser where

import Control.Monad

newtype Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) = p

instance Monad Parser where
  return x = Parser (\cs -> Just (x, cs))
  p >>= f = Parser (\cs -> case runParser p cs of
                       Nothing -> Nothing
                       Just (x, xs) -> runParser (f x) xs)

instance MonadPlus Parser where
  mzero = Parser (\_ -> Nothing)
  p `mplus` q = Parser (\cs -> case runParser p cs of
                           Nothing -> runParser q cs
                           other -> other)

get :: Parser Char
get = Parser (\cs -> case cs of
                 [] -> Nothing
                 (x:xs) -> Just (x, xs))

satisfy :: (Char -> Bool) -> Parser String
satisfy p = do x <- get
               if p x then return (x:[]) else mzero
