{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Prelude hiding (head)
import Text.Blaze
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import qualified Settings as Settings

mainMenu :: Bool -> Html
mainMenu authorized = p $ do
  a ! A.href "/" $ "Bookmarks"
  _ <- " "
  if authorized
    then a ! A.href "/logout" $ "Logout"
    else do a ! A.href "/login" $ "Login"
            _ <- " "
            a ! A.href "/register" $ "Register"

index :: Html
index =
  html $ do
    head $ do
      title (toHtml Settings.name)
    body $ do
      h1 (toHtml Settings.name)
      h2 (toHtml Settings.slogan)
      mainMenu False
      p "Some Text!"
