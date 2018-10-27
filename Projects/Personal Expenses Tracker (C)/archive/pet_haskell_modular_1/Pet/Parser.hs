module Pet.Parser (parseExpenseList) where

import Pet.Expense
import Pet.Errors
import Extras.Data.Time.Calendar
import Extras.Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.List

parseExpenseList :: String -> Either [Expense] ParserError
parseExpenseList s = 
  if _res == [] then Right $ CannotParseDocError
  else
    if length errs > 0 then Right $ CannotParseLineError $ length res
    else
      if and datesValidity then Left $ map fromJust $ filter isJust res
      else Right $ InvalidDateError $ (fromJust $ elemIndex False datesValidity) + 1
  where _res          = readP_to_S expenseList s
        (res, errs)   = last $ _res
        datesValidity = datesValid res

datesValid :: [Maybe Expense] -> [Bool]
datesValid = map (
  \x -> case x of
    Nothing -> True
    Just y  -> dateValid (yearOf y) (monthOf y) (dayOf y))

expenseList :: ReadP ([Maybe Expense])
expenseList = sepBy (choice [expenseBrackets, emptyLine]) $ char '\n'

expenseBrackets :: ReadP (Maybe Expense)
expenseBrackets = do skipWhitespace
                     y <- between (char '{') (char '}') expense
                     skipWhitespace
                     return y

expense :: ReadP (Maybe Expense)
expense = do skipWhitespace
             amount <- double
             skipWhitespace1
             (year, month, day) <- date
             skipWhitespace1
             skipCommas
             tags <- tagList
             skipCommas
             skipWhitespace1
             note <- quotedString
             skipWhitespace
             return $ Just $ Expense { amountOf = amount
                                     , yearOf   = year
                                     , monthOf  = month
                                     , dayOf    = day
                                     , tagsOf   = tags
                                     , noteOf   = note
                                     }

emptyLine :: ReadP (Maybe Expense)
emptyLine = many (satisfy (\c -> isSpace c && c /= '\n')) >> return Nothing

date :: ReadP (Integer, Int, Int)
date = do year  <- count 4 $ satisfy isNumber
          _     <- char '-'
          month <- count 2 $ satisfy isNumber
          _     <- char '-'
          day   <- count 2 $ satisfy isNumber
          return (read year, read month, read day)

tag :: ReadP String
tag = munch1 (\c -> (c /= ',') && (not $ isSpace c))

tagList :: ReadP [String]
tagList = sepBy1 tag (many1 $ char ',')
