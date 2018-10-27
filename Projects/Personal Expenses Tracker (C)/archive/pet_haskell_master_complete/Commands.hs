module Commands where

import Statistics
import Expense
import Parser
import Extras
import Errors
import Data.List
import Data.Maybe

-- arguments -> original -> filtered -> message/result
type CmdFunc = [String] -> [Expense] -> [Expense] -> Either String [Expense]

getCommandFunction :: String -> Maybe CmdFunc
getCommandFunction cmd = lookup cmd $ commandList

commandList :: [(String, CmdFunc)]
commandList = [("ls",   lsElements),
               -- ("add",  addElement),
               ("show", showElements)]

showElements :: CmdFunc
showElements []           _ _ = Left $ show $ MissingArgs "show"
showElements ["tags"]     _ l = Left $ tagStats l
showElements ["stats"]    o l = Left $ showStats o l
showElements ["months"]   _ l = Left $ monthStats l
showElements ("tags":_)   _ _ = Left $ show $ ExcessiveArgs "show tags"
showElements ("stats":_)  _ _ = Left $ show $ ExcessiveArgs "show stats"
showElements ("months":_) _ _ = Left $ show $ ExcessiveArgs "show months"
showElements (_:_)        _ _ = Left $ show $ UnknownArg "show"

addElement :: CmdFunc
addElement args _ expL
  | argsLen < 3 = Left $ show $ MissingArgs "add"
  | argsLen > 3 = Left $ show $ ExcessiveArgs "add"
  | otherwise   = case parseExpense $ unwords args of
    Right es -> Left  $ show es
    Left  e  -> Right $ resExpL e
  where argsLen   = length args
        resExpL e = expL ++ [e]

lsCommandList :: [(String, CmdFunc)]
lsCommandList = [("tags",   lsWithTags),
                 ("dates",  lsWithDates),
                 ("months", lsWithMonths),
                 ("years",  lsWithYears)]

lsElements :: CmdFunc
lsElements []  _ expL = Right expL
lsElements [x] _ _    = case lookup x lsCommandList of
  Nothing -> Left $ show $ UnknownArg "ls"
  Just _  -> Left $ show $ MissingArgs x
lsElements (x:xs) _ expL = case lookup x lsCommandList of
  Nothing -> lsElements [x] [] []
  Just f  -> f xs [] expL

lsWithDates :: CmdFunc
lsWithDates dates_ _ l
  | (length $ filter (== []) $ dates1) > 0 = Left $ show ParserErrDates
  | (length $ resL) > 0                    = Right resL
  | otherwise                              = Left $ show NoEntries
  where dates1 = map (maybeToList . parseDate) dates_
        dates  = map (!! 0) dates1
        resL   = foldl (\a e -> a ++ filterByDate e l) [] dates

lsWithMonths :: CmdFunc
lsWithMonths months_ _ expL
  | (length $ filter (== []) $ months1) > 0 = Left $ show ParserErrMonths
  | (length $ resL) > 0                     = Right resL
  | otherwise                               = Left $ show NoEntries
  where months1 = map (maybeToList . parseMonth) months_
        months  = map (!! 0) months1
        resL    = filter (\e -> (yearOf e `elem` map yearFrom months) &&
                                (monthOf e `elem` map monthFrom months)) expL

lsWithYears :: CmdFunc
lsWithYears years_ _ expL
  | (length $ filter (== []) $ years1) > 0 = Left $ show ParserErrYears
  | (length $ resL) > 0                    = Right resL
  | otherwise                              = Left $ show NoEntries
  where years1 = map (maybeToList . parseYear) years_
        years  = map (!! 0) years1
        resL   = filter (\e -> yearOf e `elem` map yearFrom years) expL

lsWithTags :: CmdFunc
lsWithTags tags_ _ l =
  if (length $ resL) > 0 then Right $ sort resL else Left $ show NoEntries
  where resL = foldl (\a e -> a ++ filterByTag e l) [] tags_
