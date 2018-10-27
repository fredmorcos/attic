module TakeWhileParser where

import Data.Char
import Data.List

type SatisfyParser = Maybe (String, String) -> (Char -> Bool) -> Maybe (String, String)

(|*) :: SatisfyParser
(|*) Nothing _ = Nothing
(|*) (Just (tok, rem)) p = Just (tok ++ newTok, newRem)
  where (newTok, newRem) = span p rem

(|.) :: SatisfyParser
(|.) Nothing _ = Nothing
(|.) (Just (tok, r:rs)) p = if p r then Just (tok ++ [r], rs) else Nothing

(|?) :: SatisfyParser
(|?) Nothing _ = Nothing
(|?) x@(Just (tok, r:rs)) p = if p r then Just (tok ++ [r], rs) else x

(|+) :: SatisfyParser
(|+) Nothing _ = Nothing
(|+) x p = x |. p |* p
