module Parser where

import Expense
import StringUtils
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data ParseError = ParseError { line :: Int
                             , col  :: Int
                             , snip :: String
                             }

instance Show ParseError where
  show (ParseError l c s) = format "Error at line {0}, column {1}: {2}"
    [show l, show c, s]

parseSpaces :: ReadP ()
parseSpaces = do _ <- munch isSpace
                 return ()

parseInt :: ReadP Double
parseInt = do a <- munch1 isDigit
              return ((read a) :: Double)

parseDouble :: ReadP Double
parseDouble = do a <- munch1 isDigit
                 b <- satisfy (== '.')
                 c <- munch1 isDigit
                 return $ read (a ++ [b] ++ c)

getAmount :: ReadP Double
getAmount = parseDouble <++ parseInt

getDate :: ReadP (Integer, Int, Int)
getDate = do y <- munch1 isDigit
             _ <- satisfy (== '-')
             m <- munch1 isDigit
             _ <- satisfy (== '-')
             d <- munch1 isDigit
             return (read y, read m, read d)

parseTag :: Bool -> ReadP String
parseTag True  = munch1 (\c -> not $ (isSpace c || c == ','))
parseTag False = do _ <- satisfy (== ',')
                    parseTag True

getTags :: ReadP [String]
getTags = do h <- parseTag True
             t <- many $ parseTag False
             return (h:t)

getNote :: ReadP String
getNote = do _ <- satisfy (== '"')
             n <- munch (/= '"')
             _ <- satisfy (== '"')
             return n

getExpense :: ReadP Expense
getExpense = do _ <- parseSpaces
                a <- getAmount
                _ <- parseSpaces
                d <- getDate
                _ <- parseSpaces
                t <- getTags
                _ <- parseSpaces
                n <- getNote
                _ <- parseSpaces
                return Expense { amountOf = a
                               , dateOf   = d
                               , tagsOf   = t
                               , noteOf   = n
                               }

parseExpenses :: ReadP [Expense]
parseExpenses = many getExpense

-- FIXME Extremely inefficient and messy, but good enough for now
getExpenses :: String -> Either ParseError [Expense]
getExpenses xs
  | null unparsed = Right res
  | otherwise = Left $ ParseError { line = l
                                  , col  = c
                                  , snip = s
                                  }
  where (res, unparsed) = last $ readP_to_S parseExpenses $ xs
        parsed = take (length xs - length unparsed) xs
        parsedL = lines parsed
        lastParsedL = last parsedL
        l = length parsedL
        -- +2: 1 for starting line index at 1, 1 for next (unparsed) char
        c = length lastParsedL + 2
        s = (take 10 lastParsedL) ++ "..." ++ (take n unparsed) ++ "..."
          where n = min 10 (case elemIndex '\n' unparsed of
                               Nothing -> 10
                               Just x -> x)
