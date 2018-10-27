module PetParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Expense
import Parser

type Line = Int
type Col  = Int
type Pos  = (Line, Col)
type PetParser b = Parser Char Pos b

updatePos :: Pos -> Char -> Pos
updatePos (l, c) i = if i == '\n' then (l + 1, 1) else (l, c + 1)

parseInt :: PetParser Int
parseInt = do v <- pSatMany1 (isDigit, "Digit")
              return (read v :: Int)

parseDouble :: PetParser Double
parseDouble = do
  d <- pSatMany1 (isDigit,  "Digit from a Double")
  _ <- pSat1     ((== '.'), "Decimal point from a Double")
  f <- pSatMany1 (isDigit,  "Digit from a Double")
  return (read (d ++ "." ++ f) :: Double)

parseAmount :: PetParser Double
parseAmount = parseDouble <|>
              (parseInt >>= \v -> return (fromIntegral v :: Double)) <|>
              failList ["Invalid value for `amount`",
                        "Expecting a number: Integer or Double"]

parseDate :: PetParser (Int, Int, Int)
parseDate = do
  p1 <- pos

  t1 <- pSat1 (isDigit,  "First digit of year")
  t2 <- pSat1 (isDigit,  "Second digit of year")
  t3 <- pSat1 (isDigit,  "Third digit of year")
  t4 <- pSat1 (isDigit,  "Fourth digit of year")

  let y = read [t1, t2, t3, t4] :: Int
  when (y < 0) $ failListPos p1
    ["Invalid value for `year`: Expecting it to be > 0"]

  _  <- pSat1 ((== '-'), "First - of date format")

  p2 <- pos

  t5 <- pSat1 (isDigit,  "First digit of month")
  t6 <- pSat1 (isDigit,  "Second digit of month")

  let m = read [t5, t6] :: Int
  when (m < 1 || m > 12) $ failListPos p2
    ["Invalid value for `month`: Expecting it to be >= 1 and <= 12"]

  _  <- pSat1 ((== '-'), "Second - of date format")

  p3 <- pos

  t7 <- pSat1 (isDigit,  "First digit of day")
  t8 <- pSat1 (isDigit,  "Second digit of day")

  let d = read [t7, t8] :: Int
      nDays = monthLength (isLeapYear $ toInteger y) m
  when (d < 1 || d > nDays) $ failListPos p3
    ["Invalid value for `day`:",
     "Expecting it to be >= 1 and <= " ++ show nDays,
     "  for the month of " ++ monthName m]

  return (y, m, d)
  where monthName n =
          ["January",   "February", "March",    "April",
           "May",       "June",     "July",     "August",
           "September", "October",  "November", "December"] !! (n - 1)

parseName :: String -> PetParser String
parseName s = pSatMany1 (\c -> c /= ',' && (not . isSpace) c, s)

parsePerson, parseShop, parseTag :: PetParser String
parsePerson = parseName "Expecting a person name"
parseShop   = parseName "Expecting a shop name"
parseTag    = parseName "Expecting a tag or a list of tags"

parseTags :: PetParser [String]
parseTags = parseTag >>= \t -> isEnd >>= \e ->
  if e then return [t] else
    peek >>= \p -> if p /= ',' then return [t] else
      item >> parseTags >>= \ts -> return $ t:ts

parseNote :: PetParser String
parseNote = pSatMany ((/= '\n'), "A note -> any text until a newline")

skipSpaces :: PetParser String -> PetParser ()
skipSpaces = ($) void

parseExpense :: PetParser Expense
parseExpense = do
  amount' <- parseAmount ; skipSpaces $ pSatMany1 onlySpace
  date'   <- parseDate   ; skipSpaces $ pSatMany1 onlySpace
  person' <- parsePerson ; skipSpaces $ pSatMany1 onlySpace
  shop'   <- parseShop   ; skipSpaces $ pSatMany1 onlySpace
  tags'   <- parseTags   ; skipSpaces $ pSatMany  onlySpace
  note'   <- parseNote
  return Expense { amount = amount' , person = person'
                 , shop   = shop'   , date   = date'
                 , tags   = tags'   , note   = note'
                 }
  where onlySpace = (\c -> isSpace c && c /= '\n', "A spacing character")

parseManyExpenses :: PetParser [Expense]
parseManyExpenses = satMany (parseExpense >>= \e ->
  skipSpaces (pSatMany (isSpace, "A spacing char or newline")) >>
  return e)
