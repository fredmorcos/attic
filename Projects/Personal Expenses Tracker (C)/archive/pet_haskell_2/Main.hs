{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main :: IO ()
main = scotty 3000 $
  get "/" $
    text "Hello World!"
