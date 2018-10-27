import Data.Char

main :: IO ()
main = interact foo

foo :: String -> String
-- foo s = map toUpper s
foo = unwords . words