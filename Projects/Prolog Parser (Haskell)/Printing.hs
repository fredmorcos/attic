module Printing where

import PrologParse

showPLVar :: PLVar -> String
showPLVar (PLSingletonVar n v     ) = n ++ " = " ++ v
showPLVar (PLListVar      n []    ) = n ++ " = []"
showPLVar (PLListVar      n [x]   ) = n ++ " = [" ++ x ++ "]"
showPLVar (PLListVar      n [x, y]) = n ++ " = [" ++ x ++ "," ++ y ++ "]"
showPLVar (PLListVar      n vs    ) =
  n ++ " = [" ++ x ++ "," ++ concatMap (++ ",") middle ++ y ++ "]"
  where x = head vs
        y = last vs
        middle = tail $ init vs

showPLState :: PLState -> String
showPLState state = unlines $ map showPLVar state

showPLStates :: [PLState] -> String
showPLStates states = unlines $ map showPLState states
