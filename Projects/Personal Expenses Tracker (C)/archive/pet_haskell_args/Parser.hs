module Parser where

import           Control.Monad (void, when)
import           Data.Char     (isDigit, isSpace)
import           Data.Functor  ((<$>))
import           Expense

-- A Character Predicate is a tuple composed of 1) A function that
-- takes a Character and returns a Boolean of whether that Character
-- satisfies it (the predicate) and 2) A String describing the
-- predicate, used for more meaningful error messages.
type CharPredicate = (Char -> Bool, String)

-- A Parser Error is composed of a String representing the error
-- message, and the line and column (both Ints) at which the error may
-- have happened.
type ParserError = (String, Int, Int)

-- A Parser Return value is Either a Parser Error (Left) or a success
-- (Right). A success is composed of an instance of a type, the
-- remainder of the input String the Parser is traversing (parsing),
-- and the current line and column (both Ints) the Parser is at.
type ParserReturn a = Either ParserError (a, String, Int, Int)

-- A Parser is a higher-order type encapsulating a function that takes
-- an input String, and current line and column numbers and returns an
-- instance of the type constructed by parsing the head of the input
-- String, the remainder of that input String and the resulting
-- current line and column numbers.
newtype Parser a = Parser (String -> Int -> Int -> ParserReturn a)

runParser :: Parser a -> String -> Int -> Int -> ParserReturn a
runParser (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser $ \i l c ->
                      (\(x, r, l', c') -> (f x, r, l', c')) <$> p i l c

instance Monad Parser where
  return v = Parser $ \i l c -> Right (v, i, l, c)
  fail msg = Parser $ \_ l c -> Left (msg, l, c)
  p >>= f  = Parser $ \i l c -> runParser p i l c >>=
                      (\(x, r, l', c') -> runParser (f x) r l' c')

-- If the parser on the left is not satisfied, then retry with the
-- parser on the right. Otherwise return whatever result came back
-- from the parser on the left. If the parser on right also does not
-- satisfy, return its error.
(<++) :: Show a => Parser a -> Parser a -> Parser a
Parser p1 <++ Parser p2 = Parser $ \i l c ->
  case p1 i l c of Left  _ -> p2 i l c
                   Right x -> Right x

peek, item :: Parser Char
peek = Parser $ \i@(x:_) l c -> Right (x, i, l, c)
item = Parser $ \(x:xs)  l c -> Right (x, xs, line l x, col c x)
  where line n c = if c == '\n' then n + 1 else n
        col  n c = if c == '\n' then 1     else n + 1

failEOF :: String -> Parser ()
failEOF msg = eof >>= \e -> when e $ fail $ "Unexpected end of input: " ++ msg

eof :: Parser Bool
eof = Parser $ \i l c -> return (null i, i, l, c)

satisfy :: CharPredicate -> Parser Char
satisfy (p, d) = do
  failEOF d ; c <- peek
  if p c then item else fail $ "Predicate not satisfied: " ++ d

satisfyMany :: CharPredicate -> Parser String
satisfyMany pd@(p, _) = do
  e <- eof; if e then return []
            else do c <- peek; if not $ p c then return []
                               else do i <- item; r <- satisfyMany pd
                                       return $ i:r

satisfyMany1 :: CharPredicate -> Parser String
satisfyMany1 pd = do c <- satisfy pd; r <- satisfyMany pd; return $ c:r

parseInt :: Parser Int
parseInt = satisfyMany1 (isDigit, "Digit") >>= (\v -> return (read v :: Int))

parseDouble :: Parser Double
parseDouble = do d <- satisfyMany1 (isDigit,  "Digit from a Double")
                 _ <- satisfy      ((== '.'), "Decimal point from a Double")
                 f <- satisfyMany1 (isDigit,  "Digit from a Double")
                 return (read (d ++ "." ++ f) :: Double)

parseAmount :: Parser Double
parseAmount = parseDouble <++
              (parseInt >>= (\v -> return (fromIntegral v :: Double)))

parseDate :: Parser (Int, Int, Int)
parseDate = do
  t1 <- satisfy (isDigit,  "First digit of year")
  t2 <- satisfy (isDigit,  "Second digit of year")
  t3 <- satisfy (isDigit,  "Third digit of year")
  t4 <- satisfy (isDigit,  "Fourth digit of year")
  _  <- satisfy ((== '-'), "First - of year format")
  t5 <- satisfy (isDigit,  "First digit of month")
  t6 <- satisfy (isDigit,  "Second digit of month")
  _  <- satisfy ((== '-'), "Second - of year format")
  t7 <- satisfy (isDigit,  "First digit of day")
  t8 <- satisfy (isDigit,  "Second digit of day")
  let y = read [t1, t2, t3, t4] :: Int
      m = read [t5, t6] :: Int
      d = read [t7, t8] :: Int
  if y < 0 then fail "Invalid year"
  else if m < 1 || m > 12 then fail "Invalid month"
       else if d < 1 || d > 31 then fail "Invalid day"
            else return (y, m, d)

parseName :: String -> Parser String
parseName s = satisfyMany1 (\c -> c /= ',' && (not . isSpace) c, s)

parsePerson, parseShop, parseTag :: Parser String
parsePerson = parseName "Expecting a person name"
parseShop   = parseName "Expecting a shop name"
parseTag    = parseName "Expecting a tag or a list of tags"

parseTags :: Parser [String]
parseTags = do t <- parseTag; e <- eof
               if e then return [t]
               else do p <- peek
                       if p /= ',' then return [t]
                       else item >> parseTags >>= (\ts -> return $ t:ts)

parseNote :: Parser String
parseNote = satisfyMany ((/= '\n'), "A Note - anything until a newline")

skipSpaces :: Parser String -> Parser ()
skipSpaces = ($) void

parseExpense :: Parser Expense
parseExpense = do amount' <- parseAmount ; skipSpaces $ satisfyMany1 onlySpace
                  date'   <- parseDate   ; skipSpaces $ satisfyMany1 onlySpace
                  person' <- parsePerson ; skipSpaces $ satisfyMany1 onlySpace
                  shop'   <- parseShop   ; skipSpaces $ satisfyMany1 onlySpace
                  tags'   <- parseTags   ; skipSpaces $ satisfyMany  onlySpace
                  note'   <- parseNote
                  return Expense { amount = amount' , person = person'
                                 , shop   = shop'   , date   = date'
                                 , tags   = tags'   , note   = note'
                                 }
  where onlySpace = (\c -> isSpace c && c /= '\n', "A spacing character")

parseManyExpenses :: Parser [Expense]
parseManyExpenses = do
  e <- eof
  if e then return []
  else do ex <- parseExpense
          skipSpaces $ satisfyMany (isSpace, "A spacing char or newline")
          exs <- parseManyExpenses
          return $ ex:exs
