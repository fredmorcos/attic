module DateHelper (parseDate) where

import Data.Time.Calendar (Day)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

parseDate :: String -> String -> Day
parseDate s e = case res of
  Nothing  -> error e
  (Just y) -> y
  where res = parseTime defaultTimeLocale "%Y-%m-%d" s
