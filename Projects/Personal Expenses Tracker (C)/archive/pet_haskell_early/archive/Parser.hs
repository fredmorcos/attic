module Parser where

import Index
import Extras

data ParserStage = PersonSection | ShopSection | YearSection | MonthSection
                 | ExpenseSection

data ParserState = ParserState { psText  :: String
                               , psLine  :: Int
                               , psCol   :: Int
                               , psTok   :: String
                               , psToks  :: [String]
                               , psIndex :: Index
                               , psStage :: ParserStage
                               }

data ParserErrorCode = Undefined
                     | Unknown { character :: Char }
                     | UnexpectedChar { character :: Char }
                     | EndOfFile

data ParserError = ParserError { peLine  :: Int
                               , peCol   :: Int
                               , peTok   :: String
                               , peStage :: ParserStage
                               , peError :: ParserErrorCode
                               }

instance Show ParserStage where
  show PersonSection = "Persons"
  show ShopSection = "Shops"
  show YearSection = "Year"
  show MonthSection = "Month"
  show ExpenseSection = "Expense"

instance Show ParserErrorCode where
  show Undefined          = "Undefined error"
  show (Unknown c)        = "Unknown error at character: " ++ [c]
  show (UnexpectedChar c) = "Unexpected character: " ++ [c]
  show EndOfFile          = "End of file"

instance Show ParserError where
  show ParserError { peLine  = line
                   , peCol   = col
                   , peTok   = tok
                   , peStage = stage
                   , peError = err
                   } = unlines ["Line " ++ show line ++ ", Column " ++ show col ++
                                ", Token " ++ tok ++ ", Stage " ++ show stage,
                                " -> " ++ show err]

errorFromState :: ParserState -> ParserError
errorFromState state = ParserError { peLine  = psLine state
                                   , peCol   = psCol state
                                   , peTok   = psTok state
                                   , peStage = psStage state
                                   , peError = Undefined
                                   }

consume :: Bool -> (Char -> Bool) -> ParserState -> Either ParserError ParserState
consume discard predicate state =
  if not $ null t
  then if predicate c
       then Right state
         { psText = cs
         , psTok  = if discard then psTok state else psTok state ++ [c]
         , psCol  = psCol state + 1
         , psLine = if isNewline c then psLine state + 1 else psLine state
         }
       else Left $ (errorFromState state) { peError = UnexpectedChar c }
  else Left $ (errorFromState state) { peError = EndOfFile }
  where t@(c:cs) = psText state

--(<~>) :: Either ParserError ParserState -> Either ParserError ParserState
--(<~>) (Left e) = e
