module QueryHandler where

import PrologParse
import ArgsHandler

varMatch :: PLVar -> PLVar -> Bool
varMatch (PLSingletonVar n1 v1) (PLSingletonVar n2 v2) = n1 == n2 && v1 == v2
varMatch (PLSingletonVar n1 v1) (PLListVar n2 v2) = n1 == n2 && v1 `elem` v2
varMatch (PLListVar _ _) (PLSingletonVar _ _) = False
varMatch (PLListVar n1 v1) (PLListVar n2 v2) = n1 == n2 && all (`elem` v2) v1

isVarInState :: PLVar -> PLState -> Bool
isVarInState v = any (\y -> v `varMatch` y)

stateMatch :: PLState -> PLState -> Bool
stateMatch s1 s2 = any (`isVarInState` s2) s1

executeQuery :: [String] -> [PLState] -> Either String [PLState]
executeQuery args states = do
  argState <- parseArgs args
  return $ filter (stateMatch argState) states
