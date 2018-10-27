module PetParser where

import Control.Applicative
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Char
import Parser

import qualified Expense as E

type Location = (Int, Int)

monthName :: Int -> String
monthName i = ["January", "February", "March",
               "April", "May", "June", "July",
               "August", "September", "October",
               "November", "December"] !! i

errInvalidMonth :: Int -> String
errInvalidMonth m = unwords ["| Invalid month value", show m]

errInvalidDay :: Int -> Int -> String
errInvalidDay d m = unwords ["| Invalid day value", show d, "of", monthName m]

errInvalidTagSep :: Char -> String
errInvalidTagSep s = unwords ["| Invalid tag separator", show s]

newline :: Char
newline = '\n'

space :: Char -> Bool
space c = isSpace c && c /= newline

locUpd :: Char -> Location -> Location
locUpd e (l, c) = if e == newline then (l + 1, 1) else (l, c + 1)

spaces :: String -> Parser Char Location String
spaces e = some $ satisfy space e

newlines :: String -> Parser Char Location String
newlines e = some $ satisfy (== newline) e

whites :: String -> Parser Char Location String
whites e = some $ satisfy isSpace e

optionalSpaces :: String -> Parser Char Location String
optionalSpaces e = many $ satisfy space e

optionalNewlines :: String -> Parser Char Location String
optionalNewlines e = many $ satisfy (== newline) e

optionalWhites :: String -> Parser Char Location String
optionalWhites e = many $ satisfy isSpace e

int :: String -> Parser Char Location Int
int e = rawInt e >>= \r -> return (read r :: Int)

intAsDouble :: Parser Char Location Double
intAsDouble = rawInt "an integer value" >>= \r -> return (read r :: Double)

rawInt :: String -> Parser Char Location String
rawInt e = some $ satisfy isDigit e

onlyDouble :: Parser Char Location Double
onlyDouble = do
  d <- rawInt "the decimal part of a double value"
  p <- satisfy (== '.') "the floating-point of a double value"
  f <- rawInt "the fractional part of a double value"
  return (read (d ++ [p] ++ f) :: Double)

amount :: Parser Char Location Double
amount = onlyDouble <|> intAsDouble

year :: Parser Char Location Int
year = do a <- satisfy isDigit "the millenia digit of year"
          b <- satisfy isDigit "the century digit of year"
          c <- satisfy isDigit "the decade digit of year"
          d <- satisfy isDigit "the yearly digit of year"
          return (read [a, b, c, d] :: Int)

month :: Parser Char Location Int
month = do l <- loc
           a <- satisfy isDigit "the first digit of month"
           b <- satisfy isDigit "the second digit of month"
           let m = read [a, b] :: Int
           if m > 12
             then err (Just l) $ errInvalidMonth m
             else return m

day :: Parser Char Location Int
day = do a <- satisfy isDigit "the first digit of day"
         b <- satisfy isDigit "the second digit of day"
         return (read [a, b] :: Int)

date :: Parser Char Location (Int, Int, Int)
date = do y <- year  ; _ <- satisfy (== '-') "the first date separator"
          m <- month ; _ <- satisfy (== '-') "the second date separator"
          l <- loc
          d <- day
          if d > monthLength (isLeapYear (toInteger y)) m
            then err (Just l) $ errInvalidDay m d
            else return (y, m, d)

name :: String -> Parser Char Location String
name e = some $ satisfy (not . isSpace) e

person :: Parser Char Location String
person = name "a person name"

shop :: Parser Char Location String
shop = name "a shop name"

tag :: Parser Char Location String
tag = some $ satisfy (\c -> not (isSpace c) && c /= ',') "a tag name"

tags :: Parser Char Location [String]
tags = tags_ [] >>= \ts -> return $ reverse ts
  where tags_ a = do t <- tag
                     e <- eof
                     if e then return $ t:a
                       else do p <- peek "a spacing"
                               if isSpace p then return $ t:a
                                 else if p == ',' then item >> tags_ (t:a)
                                      else err Nothing $ errInvalidTagSep p

note :: Parser Char Location (Maybe String)
note = do r <- many $ satisfy (/= newline) "anything until a newline"
          return $ if null r then Nothing else Just r

expense :: Parser Char Location E.Expense
expense = do a <- amount <|> err Nothing "Error parsing Amount"
             _ <- spaces "the spacing after the amount"
             d <- date <|> err Nothing "Error parsing Date"
             _ <- spaces "the spacing after the date"
             p <- person <|> err Nothing "Error parsing Person Name"
             _ <- spaces "the spacing after the person name"
             s <- shop <|> err Nothing "Error parsing Shop Name"
             _ <- spaces "the spacing after the shop name"
             t <- tags <|> err Nothing "Error parsing Tags"
             _ <- optionalSpaces "the spacing after the tags"
             n <- note <|> err Nothing "Error parsing Note"
             return E.Expense { E.amount = a , E.date = d
                              , E.person = p , E.shop = s
                              , E.tags = t   , E.note = n
                              }

expenses :: Parser Char Location [E.Expense]
expenses = expenses_ [] >>= \es -> return $ reverse es
  where optWhites = optionalWhites "a spacing between expenses"
        expenses_ a = optWhites >> eof >>= \e ->
          if e then return a else expense >>= \i ->
          optionalWhites "a spacing between expenses" >> expenses_ (i:a)
