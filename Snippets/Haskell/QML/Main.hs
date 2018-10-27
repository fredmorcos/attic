module Main where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Graphics.QML

factorial :: Integer -> Integer
factorial n = product [1..n]

main :: IO ()
main = do clazz <- newClass [
            defMethod' "factorial" (\_ txt ->
                                     let n = read $ T.unpack txt :: Integer
                                     in return . T.pack . show $ product [1..n] :: IO Text)]
          ctx <- newObject clazz ()
          runEngineLoop defaultEngineConfig {
            initialDocument = fileDocument "Factorial_1.qml",
            contextObject = Just $ anyObjRef ctx }
