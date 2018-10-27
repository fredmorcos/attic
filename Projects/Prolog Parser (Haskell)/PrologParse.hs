module PrologParse where

import Control.Applicative
import Control.Monad
import Data.Char

import SimpleParse

data PLVar
  = PLSingletonVar { name :: String, value  :: String   }
  | PLListVar      { name :: String, values :: [String] }
  deriving Show

type PLState = [PLVar]

skipSpaces :: Parser Char ()
skipSpaces = void $ pSatMany1 $ \c -> isSpace c && c /= '\n'

skipEqual :: Parser Char ()
skipEqual = void $ pSat (== '=')

skipOpenBracket :: Parser Char ()
skipOpenBracket = void $ pSat (== '[')

skipCloseBracket :: Parser Char ()
skipCloseBracket = void $ pSat (== ']')

skipComma :: Parser Char ()
skipComma = void $ pSat (== ',')

parseNewlines :: Parser Char Int
parseNewlines = pSatMany1 (== '\n') >>= \l -> return $ length l

parseVarName :: Parser Char String
parseVarName = do
  x <- pSat isUpper
  xs <- pSatMany $ \c -> isAlphaNum c || c == '_'
  return $ x:xs

parseVarValueSingleton :: Parser Char String
parseVarValueSingleton = do
  x <- pSat isLower
  xs <- pSatMany $ \c -> isAlphaNum c || c == '_'
  return $ x:xs

parseVarValueListElements :: Parser Char [String]
parseVarValueListElements = do
  s <- peek
  case s of
   ']' -> return []
   ',' -> skipComma >> parseVarValueListElements
   _   -> do
     x <- parseVarValueSingleton
     xs <- parseVarValueListElements
     return $ x:xs

parseVarValueList :: Parser Char [String]
parseVarValueList = do
  skipOpenBracket
  xs <- parseVarValueListElements
  skipCloseBracket
  return xs

parseVarValue :: Parser Char (Either [String] String)
parseVarValue = (parseVarValueList      >>= \x -> return $ Left x) <|>
                (parseVarValueSingleton >>= \x -> return $ Right x)

parseVar :: Parser Char PLVar
parseVar = do
  n <- parseVarName
  skipSpaces >> skipEqual >> skipSpaces
  v <- parseVarValue
  return $ case v of
   Left list -> PLListVar { name = n, values = list }
   Right single -> PLSingletonVar { name = n, value = single }

parseVarGroup :: Parser Char PLState
parseVarGroup = do
  x <- parseVar
  nls <- parseNewlines
  if nls == 1
    then parseVarGroup >>= \xs -> return $ x:xs
    else return [x]

parseVarGroups :: Parser Char [PLState]
parseVarGroups = satMany parseVarGroup
