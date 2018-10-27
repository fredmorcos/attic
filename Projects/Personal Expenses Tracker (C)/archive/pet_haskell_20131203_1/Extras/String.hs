module Extras.String where

bracketize :: String -> String
bracketize s = "[" ++ s ++ "]"

stamp :: String -> String -> String
stamp stp msg = bracketize stp ++ " " ++ msg
