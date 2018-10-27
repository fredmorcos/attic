module VCard.VCard where

import Control.Monad
import Control.Applicative
import SimpleParse.Parser

data Field = Field { fieldName       :: String
                   , fieldProperties :: [String]
                   , fieldValues     :: [String]
                   }

type Person = [Field]

parseBegin :: Parser Char ()
parseBegin = void $ expectSeq "BEGIN:VCARD"

parseVersion :: Parser Char Double
parseVersion = expectSeq "VERSION:" >> pUntil

parseEnd :: Parser Char ()
parseEnd = void $ expectSeq "END:VCARD"

parsePersonVCardVersion :: Parser Char Double
parsePersonVCardVersion = do void $ expectSeq "VERSION:"
                             x <- pUntil (expect '\n') item
                             return $ read x

parsePersonName :: Parser Char [String]
parsePersonName = do void $ expectSeq "N:"
                     x <- pUntil (expect ';' <|> expect '\n') item
                     xs <- parsePersonName
                     return $ filter (not . null) $ x:xs

parsePersonFormattedName :: Parser Char String
parsePersonFormattedName = do void $ expectSeq "FN:"
                              pUntil (expect '\n') item

parseField :: Parser Char Field
parseField = do let cellParser = pUntil (expect ';' <|> expect ':') item
                name <- cellParser
                properties <- sepBy (expect ';') cellParser
                value <- pUntil (expect '\n') item
                return Field { fieldName = name
                             , fieldProperties = properties
                             , fieldValue = value
                             }

parseVCard :: Parser Char Person
parseVCard = do parseBegin
                version <- parsePersonVCardVersion
                name <- parsePersonName
                formattedName <- parsePersonFormattedName
                fields <- satMany parseField
                parseEnd
                return Person { personVCardVersion = version
                              , personName = name
                              , personFormattedName = formattedName
                              , personFields = fields
                              }

parseVCards :: Parser Char [Person]
parseVCards = undefined
