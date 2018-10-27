module Help where

import StringUtils

help :: String -> String
help body = unlines' $ header ++ [body] ++ footer
  where header = if helpHeader == ""
                 then []
                 else [helpHeader, ""]
        footer = if helpFooter == ""
                 then []
                 else [helpFooter, ""]

helpHeader :: String
helpHeader = unlines' [ "pet version 2.0"
                      , "usage: pet FILE COMMAND [ARGUMENTS...]"
                      ]

helpFooter :: String
helpFooter = ""
