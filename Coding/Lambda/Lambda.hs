import Data.Char

skipWhiteSpaces :: String -> String
skipWhiteSpaces (c:cs) | isSpace c = skipWhiteSpaces cs
skipWhiteSpaces cs = cs

nextWordAndRest :: String -> (String, String)
nextWordAndRest text = nextWordAndRest' (skipWhiteSpaces text) ""
  where
    nextWordAndRest' :: String -> String -> (String, String)
    nextWordAndRest' (s:rest) word | isSpace s = (reverse word, rest)
    nextWordAndRest' rest@('(':_) word = (reverse word, rest)
    nextWordAndRest' rest@(')':_) word = (reverse word, rest)
    nextWordAndRest' [] word = (reverse word, [])
    nextWordAndRest' (c:rest) word = nextWordAndRest' rest (c:word)

-- tokenize :: String -> [String]
-- tokenize [] = []
-- tokenize text = tokenize' (skipWhiteSpaces text)
--   where
--     tokenize' :: String -> [String]
--     tokenize' ('(' : remain) = "(" : (tokenize remain)
--     tokenize' (')' : remain) = ")" : (tokenize remain)
--     tokenize' remain = word : tokenize rest
--       where (word, rest) = nextWordAndRest remain

tokenize :: String -> [String]
tokenize [] = []
tokenize text = tokenize' (skipWhiteSpaces text)
    where
        tokenize' :: String -> [String]
        tokenize' ('(' : rest) = "(" : (tokenize rest)
        tokenize' (')' : rest) = ")" : (tokenize rest)
        tokenize' rest = fst x : (tokenize (snd x))
                where x = nextWordAndRest rest

isValidExprTokens :: [String] -> (Bool, [String])
isValidExprTokens [] = (True, [])
isValidExprTokens ("(" : "+" : rest) =
  case isValidExprTokens rest of
    (True, rest') -> case isValidExprTokens rest' of
      (True, rest'') -> case rest'' of
        (")" : rest''') -> (True, rest''')
        _ -> (False, rest'')
      x -> x
    x -> x
isValidExprTokens ("(" : "-" : rest) =
  case isValidExprTokens rest of
    (True, rest') -> case rest' of
      (")" : rest'') -> (True, rest'')
      _ -> (False, rest')
    x -> x
isValidExprTokens (word@(w : _) : rest)
  | isNumber w = (isValidNum word, rest)
  | isAlpha w = (isValidAlpha word, rest)
  where
    isValidNum :: String -> Bool
    isValidNum [] = False
    isValidNum (x:[]) = isNumber x
    isValidNum (x:xs) = isNumber x && isValidNum xs
    isValidAlpha :: String -> Bool
    isValidAlpha [] = False
    isValidAlpha (x:[]) = isAlphaNum x
    isValidAlpha (x:xs) = isAlphaNum x && isValidAlpha xs
isValidExprTokens unmatched = (False, unmatched)

main :: IO ()
main = do
  print (tokenize "(+ (+ x 12) (- y))")
  print (isValidExprTokens (tokenize "(+ (+ x 12) (- y))"))
