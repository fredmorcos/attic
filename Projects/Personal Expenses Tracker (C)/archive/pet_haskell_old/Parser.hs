module Parser where

import Expense
import Index
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

class Parse a where
  parse :: ReadP a

instance Parse Double where
  parse = parseDouble

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

skipSpacesOnly :: ReadP ()
-- ^ Skips all whitespace except newline.
skipSpacesOnly = do s <- look
                    skip s
 where skip (c:s) | isSpace c && c /= '\n' = do _ <- get; skip s
       skip _                              = return ()

skipNewlinesOnly :: ReadP ()
-- ^ Skips newlines only.
skipNewlinesOnly = do s <- look
                      skip s
 where skip (c:s) | c == '\n' = do _ <- get; skip s
       skip _                 = return ()

parseDouble :: ReadP Double
parseDouble = do f <- munch1 isDigit
                 remain <- look
                 if head remain == '.'
                 then do _ <- char '.'
                         d <- munch1 isDigit
                         return (read (f ++ "." ++ d) :: Double)
                 else return (read f :: Double)

parseDocument :: ReadP Index
parseDocument = do skipSpaces
                   personSection <- parseCSVs
                   skipNewlinesOnly
                   shopSection <- parseCSVs
                   skipNewlinesOnly
                   body <- parseBody
                   skipSpaces
                   return Index { persons = map trim personSection
                                , shops = map trim shopSection
                                , index = body
                                }

parseCSVs :: ReadP [String]
parseCSVs = sepBy1 (munch1 (\x -> x /= ',' && x /= '\n' && (not . isSpace) x)) (char ',')

parseBody :: ReadP [YearIndex]
parseBody = do skipSpaces
               yearSections <- many parseYearSection
               skipSpaces
               return yearSections

parseYearSection :: ReadP YearIndex
parseYearSection = do skipSpaces
                      _ <- char 'y'
                      skipSpacesOnly
                      yearStr <- munch1 isDigit
                      skipSpaces
                      monthSection <- many parseMonthSection
                      skipSpaces
                      return YearIndex { year = read yearStr :: Int
                                       , months = monthSection
                                       }

parseMonthSection :: ReadP MonthIndex
parseMonthSection = do skipSpaces
                       _ <- char 'm'
                       skipSpacesOnly
                       monthStr <- munch1 isDigit
                       skipSpaces
                       exps <- many parseExpense
                       skipSpaces
                       return MonthIndex { month = read monthStr :: Int
                                         , expenses = exps
                                         }

parseExpense :: ReadP Expense
parseExpense = do skipSpaces
                  amountVal <- parseDouble
                  skipSpacesOnly
                  dayVal <- munch1 isDigit
                  skipSpacesOnly
                  pVal <- munch1 isDigit
                  skipSpacesOnly
                  sVal <- munch1 isDigit
                  skipSpacesOnly
                  tVal <- parseCSVs
                  skipSpacesOnly
                  nVal <- manyTill (satisfy (/= '\n')) (satisfy (== '\n'))
                  skipSpaces
                  return Expense { amount = amountVal
                                 , day = read dayVal :: Int
                                 , person = read pVal :: Int
                                 , shop = read sVal :: Int
                                 , tags = map trim tVal
                                 , note = trim nVal
                                 }
