import Extras (trim)

main :: IO ()
main = interact (parseLines . lines)

parseLines :: [String] -> Int -> String
parseLines (line:tail) num = "hello"
