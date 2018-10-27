module HSON where

import Data.Char
import Text.ParserCombinators.ReadP

comma :: ReadP ()
comma = do char ','; return ()

dot :: ReadP ()
dot = do char '.'; return ()

lCurly :: ReadP ()
lCurly = do char '{'; return ()

rCurly :: ReadP ()
rCurly = do char '}'; return ()

lSquare :: ReadP ()
lSquare = do char '['; return ()

rSquare :: ReadP ()
rSquare = do char ']'; return ()

quote :: ReadP ()
quote = do char '"'; return ()

plus :: ReadP ()
plus = do char '+'; return ()

minus :: ReadP ()
minus = do char '-'; return ()

col :: ReadP ()
col = do char ':'; return ()

document :: ReadP [a]
document = do skipSpaces
              res <- many object
              skipSpaces
              return res

-- object :: ReadP a
object = do skipSpaces
            res <- between lCurly rCurly (many members)
            skipSpaces
            return res

-- members :: ReadP [a]
members = do skipSpaces
             res <- sepBy1 pair comma
             skipSpaces
             return res

-- pair :: ReadP (String, a)
pair = do skipSpaces
          name <- jstring
          skipSpaces
          col
          skipSpaces
          val <- value
          skipSpaces
          return (name, val)

-- array :: ReadP [a]
array = do skipSpaces
           res <- between lSquare rSquare (many elements)
           skipSpaces
           return res

-- elements :: ReadP [a]
elements = do skipSpaces
              res <- sepBy1 value comma
              skipSpaces
              return res

-- value :: ReadP (ReadP
value = do skipSpaces
           res <- choice [jstring, members, object, array,
                          (string "true"), (string "TRUE"),
                          (string "false"), (string "FALSE"),
                          (string "null"), (string "NULL")]
           skipSpaces
           return res

jstring :: ReadP String
jstring = do skipSpaces
             res <- between quote quote chars
             skipSpaces
             return res

chars :: ReadP String
chars = many1 jchar

jchar :: ReadP Char
jchar = satisfy (\c -> c /= '"' && c /= '\\')

number :: ReadP Double
number = do skipSpaces
            res <- choice [int, intfrac, intexp, intfracexp]
            skipSpaces
            return $ (read res :: Double)

int :: ReadP String
int = do skipSpaces
         res <- choice [digits, minusdigits]
         return res

intfrac :: ReadP String
intfrac = do skipSpaces
             i <- int
             f <- frac
             skipSpaces
             return $ i ++ f

intexp :: ReadP String
intexp = do skipSpaces
            i <- int
            e <- jexp
            skipSpaces
            return $ i ++ e

intfracexp :: ReadP String
intfracexp = do skipSpaces
                i <- int
                f <- frac
                e <- jexp
                skipSpaces
                return $ i ++ f ++ e

digits :: ReadP String
digits = many1 $ satisfy isNumber

minusdigits :: ReadP String
minusdigits = do minus
                 res <- digits
                 return $ "-" ++ res

frac :: ReadP String
frac = do dot
          res <- digits
          return $ "." ++ res

jexp :: ReadP String
jexp = do char 'e'
          res <- digits
          return $ "e" ++ res
