import Data.Time.Calendar (Day)
import System.Environment (getArgs)
import Parser (parseExpenses)
import Functions (showExpenses)
import DateHelper (parseDate)

import qualified Data.Map as M

main :: IO ()
main = do args <- getArgs
          contents <- getContents
          let expList = parseExpenses contents
          argStart args expList
          return ()

argStartFuncs :: M.Map String ([String] -> [(Day, Double, String)] -> IO ())
argStartFuncs = M.fromList $ [("show", argShow)]

argStart :: [String] -> [(Day, Double, String)] -> IO ()
argStart [] exp = defaultFunc [] exp
  where (Just defaultFunc) = M.lookup "show" argStartFuncs
argStart (x:xs) exp = case res of
  Nothing  -> putStrLn $ "Unknown command `" ++ x ++ "' given."
  (Just y) -> y xs exp
  where res = M.lookup x argStartFuncs

argShowFuncs :: M.Map String ([String] -> [(Day, Double, String)] -> IO ())
argShowFuncs = M.fromList $ [("day",      argShowDay),
                             ("month",    argShowMonth),
                             ("category", argShowCategory),
                             ("all" ,     argShowAll)
                            ]

argShow :: [String] -> [(Day, Double, String)] -> IO ()
argShow [] exp = defaultFunc [] exp
  where (Just defaultFunc) = M.lookup "all" argShowFuncs
argShow (x:xs) exp = case res of
  Nothing  -> putStrLn $ "Unknown argument `" ++ x ++ "' given."
  (Just y) -> y xs exp
  where res = M.lookup x argShowFuncs

argShowDay :: [String] -> [(Day, Double, String)] -> IO ()
argShowDay _ _ = putStrLn "empty"

argShowMonth :: [String] -> [(Day, Double, String)] -> IO ()
argShowMonth _ _ = putStrLn "empty"

argShowCategory :: [String] -> [(Day, Double, String)] -> IO ()
argShowCategory _ _ = putStrLn "empty"

argShowAll :: [String] -> [(Day, Double, String)] -> IO ()
argShowAll [] exp = putStrLn $ showExpenses exp
argShowAll a _ = putStrLn $ "Extraneous argument(s) `" ++ (show a) ++ "' given."
