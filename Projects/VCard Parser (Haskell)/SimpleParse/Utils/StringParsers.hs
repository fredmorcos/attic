module SimpleParse.Utils.StringParsers where

import Data.Char
import Control.Monad
import SimpleParse.Parser

parseInt :: Parser Char Int
parseInt = liftM read $ pSatMany1 isDigit

parseInteger :: Parser Char Integer
parseInteger = liftM read $ pSatMany1 isDigit

parseSpaces :: Parser Char Int
parseSpaces = liftM length (pSatMany isSpace)

parseSpaces1 :: Parser Char Int
parseSpaces1 = liftM length (pSatMany1 isSpace)

parseSpacesOnly :: Parser Char Int
parseSpacesOnly = liftM length (pSatMany $ \c -> isSpace c && c /= '\n')

parseSpacesOnly1 :: Parser Char Int
parseSpacesOnly1 = liftM length (pSatMany1 $ \c -> isSpace c && c /= '\n')

parseNewlinesOnly :: Parser Char Int
parseNewlinesOnly = liftM length $ pSatMany (== '\n')

parseNewlinesOnly1 :: Parser Char Int
parseNewlinesOnly1 = liftM length $ pSatMany1 (== '\n')
