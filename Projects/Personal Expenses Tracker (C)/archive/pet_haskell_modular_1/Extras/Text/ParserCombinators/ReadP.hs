module Extras.Text.ParserCombinators.ReadP where

import Data.Char
import Text.ParserCombinators.ReadP

skipWhitespace :: ReadP ()
skipWhitespace = skipMany (satisfy (\c -> isSpace c && c /= '\n'))

skipWhitespace1 :: ReadP ()
skipWhitespace1 = skipMany1 (satisfy (\c -> isSpace c && c /= '\n'))

skipCommas :: ReadP ()
skipCommas = skipMany $ char ','

double :: ReadP Double
double = do number <- munch1 isNumber
            dec    <- option "" $ do _    <- char '.'
                                     _dec <- munch1 isNumber
                                     return $ "." ++ _dec
            return $ read $ number ++ dec

quotedString :: ReadP String
quotedString = between (char '"') (char '"') $ many $ satisfy (\c -> c /= '\n')
