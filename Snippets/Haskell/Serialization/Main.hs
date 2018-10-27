{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Serialize
import Data.Serialize.Text ()
import qualified Data.ByteString as BS

import GHC.Generics

data Expense = Expense { amount :: Double
                       , date :: (Int, Int, Int)
                       , person :: Text
                       , shop :: Text
                       , tags :: [Text]
                       , note :: Maybe Text
                       } deriving (Generic, Show)

instance Serialize Expense where

main :: IO ()
main =
  do BS.writeFile "foo" $
       encode Expense { amount = 4.5
                      , date = (2014, 7, 26)
                      , person = "Fred"
                      , shop = "Hofer"
                      , tags = ["food", "work", "lunch"]
                      , note = Nothing
                      }
     d <- BS.readFile "foo"
     print (decode d :: Either String Expense)
