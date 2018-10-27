module Parser where

import Expense
import Errors
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

parseExpenseList :: String -> [Either Expense ErrString]
parseExpenseList s = map parseExpense $ lines s

parseExpense :: String -> Either Expense ErrString
parseExpense es | pExpLen == 0    = Right ParserNothing
                | pExpLen > 3     = Right $ ParserTooMany   es
                | pExpLen < 3     = Right $ ParserTooLittle es
                | isNothing pAmnt = Right $ ParserErrAmount (pExp !! 0)
                | isNothing pDate = Right $ ParserErrDate   (pExp !! 1)
                | isNothing pTags = Right $ ParserErrTags   (pExp !! 2)
                | otherwise       = Left $ Expense {amountOf = pAmntUnp,
                                                    dateOf   = pDateUnp,
                                                    tagsOf   = pTagsUnp}
  where pExp     = words es
        pExpLen  = length pExp
        pAmnt    = parseAmount $ pExp !! 0
        pDate    = parseDate   $ pExp !! 1
        pTags    = parseTags   $ pExp !! 2
        pAmntUnp = (maybeToList pAmnt) !! 0
        pDateUnp = (maybeToList pDate) !! 0
        pTagsUnp = (maybeToList pTags) !! 0

parseAmount :: String -> Maybe Double
parseAmount s | parsedS == [] = Nothing
              | otherwise     = Just $ fst $ parsedS !! 0
  where parsedS = reads s :: [(Double, String)]

parseYear :: String -> Maybe Day
parseYear = parseTime defaultTimeLocale "%Y"

parseMonth :: String -> Maybe Day
parseMonth = parseTime defaultTimeLocale "%Y-%m"

parseDate :: String -> Maybe Day
parseDate = parseTime defaultTimeLocale "%Y-%m-%d"

parseTags :: String -> Maybe [String]
parseTags s | length res == 0 = Nothing
            | otherwise       = Just res
  where res = filter (/= "") $ __parseTags s "" []

__parseTags :: String -> String -> [String] -> [String]
__parseTags []     ct tl = tl ++ [ct]
__parseTags (x:xs) ct tl | x /= ','  = __parseTags xs (ct ++ [x]) tl
                         | otherwise = __parseTags xs "" (tl ++ [ct])
