module ErrorMessages where

incompleteParse :: Int -> Int -> String
incompleteParse l c = "Parse error (incomplete parse) at line " ++
                      show l ++ ", column " ++ show c

successfulParse :: Int -> String
successfulParse l = "Successful parse: " ++ show l ++ " expenses"

parseError :: Int -> Int -> String -> String
parseError l c m = "Parse error at line " ++ show l ++
                   ", column " ++ show c ++ ": " ++ m
