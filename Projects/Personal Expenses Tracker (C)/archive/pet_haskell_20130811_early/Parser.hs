module Parser where

import Test.QuickCheck
import Index()
import Extras

data ParserErr = NoIndexSection  | NoPersonIndexSection  | NoShopIndexSection
               | BadIndexSection | BadPersonIndexSection | BadShopIndexSection

parsePersonIndexSection :: [String] -> Either ParserErr ([String], [String])
parsePersonIndexSection [] = Left NoIndexSection
parsePersonIndexSection (x:xs) = if not (null x)
                                 then Right (ps, xs)
                                 else Left NoPersonIndexSection
                                   where ps = splitWith ',' x
