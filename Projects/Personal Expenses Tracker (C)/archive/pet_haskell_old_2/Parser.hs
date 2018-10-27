module Parser where

import           Control.Monad
import           Data.Char
import           Expense

newtype Parser a = Parser (String -> Int -> Int ->
                           Either (String, Int, Int) (a, String, Int, Int))

uncurry3 :: (a, b, c) -> (a -> b -> c -> d) -> d
uncurry3 (x, y, z) f = f x y z

runParser :: Parser a -> String -> Int -> Int ->
             Either (String, Int, Int) (a, String, Int, Int)
runParser (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser $ \i l c -> case p i l c of
    Left x -> Left x
    Right (x, r, l', c') -> Right (f x, r, l', c')

instance Monad Parser where
  return v = Parser $ \i l c -> Right (v, i, l, c)
  fail msg = Parser $ \_ l c -> Left (msg, l, c)
  p >>= f = Parser $ \i l c -> case runParser p i l c of
    Left x -> Left x
    Right (x, r, l', c') -> runParser (f x) r l' c'

updateLine :: Int -> Char -> Int
updateLine n c = if c == '\n' then n + 1 else n

updateCol :: Int -> Char -> Int
updateCol n c = if c == '\n' then 1 else n + 1

peek :: Parser Char
peek = failOnEOF >> (Parser $ \i@(x:_) l c -> Right (x, i, l, c))

item :: Parser Char
item = failOnEOF >>
       (Parser $ \(x:xs) l c -> Right (x, xs, updateLine l x, updateCol c x))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do failOnEOF; c <- peek
               if p c then item else fail "Predicate not satisfied"

satisfyString :: (Char -> Bool) -> Parser String
satisfyString p = satisfy p >>= (\c -> return [c])

failOnEOF :: Parser ()
failOnEOF = do e <- eof; when e $ fail "Unexpected end of input"

eof :: Parser Bool
eof = Parser $ \i l c -> return (if null i then (True, [], l, c)
                                 else (False, i, l, c))

satisfyMany :: (Char -> Bool) -> Parser String
satisfyMany p = do e <- eof
                   if e then return []
                     else do c <- peek
                             if not $ p c then return []
                               else compose
  where compose = do d <- item; r <- satisfyMany p; return $ d:r

satisfyMany1 :: (Char -> Bool) -> Parser String
satisfyMany1 p = do c <- satisfy p; r <- satisfyMany p; return $ c:r

(<++) :: Show a => Parser a -> Parser a -> Parser a
Parser p1 <++ Parser p2 = Parser $ \i l c -> case p1 i l c of Left _ -> p2 i l c
                                                              Right x -> Right x

parseIntAsDouble :: Parser Double
parseIntAsDouble = parseInt >>= (\v -> return (fromIntegral v :: Double))

parseInt :: Parser Int
parseInt = satisfyMany1 isDigit >>= (\v -> return (read v :: Int))

parseDouble :: Parser Double
parseDouble = do d <- satisfyMany1 isDigit
                 _ <- satisfy (== '.')
                 f <- satisfyMany1 isDigit
                 return (read (d ++ "." ++ f) :: Double)

parseAmount :: Parser Double
parseAmount = parseDouble <++ parseIntAsDouble

parseDate :: Parser (Int, Int, Int)
parseDate = do
  t1 <- satisfyString isDigit; t2 <- satisfyString isDigit
  t3 <- satisfyString isDigit; t4 <- satisfyString isDigit
  _  <- satisfyString (== '-')
  t5 <- satisfyString isDigit; t6 <- satisfyString isDigit
  _  <- satisfyString (== '-')
  t7 <- satisfyString isDigit; t8 <- satisfyString isDigit
  let y = read (t1 ++ t2 ++ t3 ++ t4) :: Int
      m = read (t5 ++ t6) :: Int
      d = read (t7 ++ t8) :: Int
  if y < 0 then fail "Invalid year"
    else if m < 1 || m > 12 then fail "Invalid month"
         else if d < 1 || d > 31 then fail "Invalid day"
              else return (y, m, d)

parseName :: Parser String
parseName = satisfyMany1 (\c -> c /= ',' && (not . isSpace) c)

parseTags :: Parser [String]
parseTags = parseName >>= (\t -> parseTags' >>= (\ts -> return (t:ts)))
  where parseTags' = do e <- eof
                        if e then return []
                          else do p <- peek
                                  if p == ',' then item >> parseTags
                                    else return []

skipSpaces :: Parser ()
skipSpaces = void $ satisfyMany isSpace

skipSpacesWithoutNewline :: Parser ()
skipSpacesWithoutNewline = void $ satisfyMany (\c -> isSpace c && c /= '\n')

skipSpaces1 :: Parser ()
skipSpaces1 = void $ satisfyMany1 isSpace

parseNote :: Parser String
parseNote = satisfyMany (/= '\n')

parseExpense :: Parser Expense
parseExpense = do amount' <- parseAmount ; skipSpaces1
                  date'   <- parseDate   ; skipSpaces1
                  person' <- parseName   ; skipSpaces1
                  shop'   <- parseName   ; skipSpaces1
                  tags'   <- parseTags   ; skipSpacesWithoutNewline
                  note'   <- parseNote
                  return Expense {
                    amount = amount', person = person', shop = shop',
                    date = date'    , tags = tags',     note = note'  }

parseManyExpenses :: Parser [Expense]
parseManyExpenses =
  eof >>= (\e -> if e then return []
                 else do ex <- parseExpense; skipSpaces
                         exs <- parseManyExpenses
                         return $ ex:exs)
