import System.Environment
import System.Directory
import System.IO
import System.Time

-- (Date, Amount, Category)
-- Date of transaction, Amount (+ or -) and Tag/Category
data Transaction = Transaction {date     :: CalendarTime,
                                amount   :: Double,
                                category :: String} deriving (Show, Read)

main :: IO ()
main = do args <- getArgs
          hPutStr stderr $ unlines $ lines $ dispatchArgs args
          return ()

-- Check the sanity of the arguments passed and return a handle to the
-- input file. Dispatches sanity checks to appropriate functions.
dispatchArgs :: [String] -> String
dispatchArgs [] = "No arguments given. Try `help'."
dispatchArgs (x:xs)
  | x == "help" = help_ xs
  | x == "add"  = add_  xs
  | x == "show" = show_ xs
  | otherwise   = "What is `" ++ x ++ "'? Try `help'."

add_ :: [String] -> String
add_ [] = "No arguments given. Try `help add'."
add_ (x:xs) = "Add called."

show_ :: [String] -> String
show_ [] = "Show called."
show_ (x:xs) = "Show called."

help_ :: [String] -> String
help_ [] = helpMessage
help_ (x:xs)
  | x == "help" = "Hehe. Slight chuckles, quite funny."
  | x == "add"  = helpAddMessage
  | x == "show" = helpShowMessage
  | otherwise   = "What is `" ++ x ++ "'? Try `help'."

helpMessage :: String
helpMessage = unlines
  ["Usage:",
   "  help   -- Display this help.",
   "  add    -- Add a transaction item.",
   "  show   -- Show transaction information.",
   "",
   "Also try `help <cmd>' to find more information",
   "about `cmd', e.g., `help create'."]

helpAddMessage :: String
helpAddMessage = unlines
  ["Usage:",
   "  add <file> transaction <date> <amount> <tag>"]

helpShowMessage :: String
helpShowMessage = unlines
  ["Usage:",
   "  show <file> all",
   "  show <file> balance"]
