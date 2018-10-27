module Parser (parseExpenses) where

import Errors (badElemErrorString, badDateErrorString)
import Data.Time.Calendar (Day)
import DateHelper (parseDate)

-- Will take the contents string (from file, stdin, etc..) and parse
-- it into a list of triplets with (date, amount, category) where date
-- is the date of the transaction (as a Day), amount is the
-- transaction amount (as a Double) and category is the category the
-- transaction should be in (as a String without spaces).
parseExpenses :: String -> [(Day, Double, String)]
parseExpenses c = map extractElem $ filter filterElem mappedLines
  where expenseLines = lines c
        mappedLines = map parseExpensesElem expenseLines
        filterElem Nothing = False
        filterElem (Just _) = True
        extractElem (Just x) = x

-- Will take the string representing a single expense 'line' and parse
-- it into 3 words: the date, the amount and the category. The date is
-- 'read' into a Day type, the amount into a Double type and the
-- category stays as-is (a String).
parseExpensesElem :: String -> Maybe (Day, Double, String)
parseExpensesElem e = if wordedElemLen == 0
                      then Nothing
                      else if wordedElemLen == 3
                           then Just (date, amount, category)
                           else error $ badElemErrorString
  where wordedElem = words e
        wordedElemLen = length $ wordedElem
        date = parseDate (wordedElem !! 0) badDateErrorString
        amount = read (wordedElem !! 1) :: Double
        category = wordedElem !! 2
