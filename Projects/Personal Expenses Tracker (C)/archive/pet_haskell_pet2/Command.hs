module Command where

import Expense
import Pretty
import StringUtils
import Help

data Result = Error      { message    :: String }
            | Output     { message    :: String }
            | FileOutput { dataResult :: [Expense] }

data Argument = Argument { argumentName :: String
                         , argumentHelp :: String
                         }

data Command = Command { commandName :: String
                       , commandHelp :: String
                       , commandFunc :: [String] -> [Expense] -> Result
                       , commandArgs :: [Argument]
                       }

instance PrettyShow Command where
  pShow (Command n h _ a) = unlines'
    (format "{0}  {1}" [n, h] : map (("  " ++ ) . pShow) a)

instance PrettyShow Argument where
  pShow (Argument n h) = format "{0}  {1}" [n, h]
  pShowList = unlines' . map (("  " ++) . pShow)

commandList :: [Command]
commandList =
  [ Command { commandName = "help"
            , commandHelp = "Display help"
            , commandFunc = \_ _ -> Error { message = help $ pShowList commandList }
            , commandArgs = []
            }
  , Command { commandName = "add"
            , commandHelp = "Add an expense element"
            , commandFunc = \_ _ -> Output { message = "Called add element" }
            , commandArgs =
              [ Argument { argumentName = "amount"
                         , argumentHelp = "Expense amount"
                         }
              , Argument { argumentName = "date"
                         , argumentHelp = "Date of expense transaction"
                         }
              , Argument { argumentName = "tags"
                         , argumentHelp = "Categories of expense"
                         }
              , Argument { argumentName = "note"
                         , argumentHelp = "Note about expense"
                         }
              ]
            }
  ]
