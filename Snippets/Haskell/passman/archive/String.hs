module String (splitString) where

splitString :: String -> Char -> [String]
splitString s c = splitStringHelper s c ""

splitStringHelper :: String -> Char -> String -> [String]
splitStringHelper "" _ css = if css /= "" then [css] else []
splitStringHelper (x:xs) c css = 
  if x == c && css /= "" then
     css : splitStringHelper xs c ""
  else
     splitStringHelper xs c (css ++ a) where a = if x /= c then [x] else ""
     
