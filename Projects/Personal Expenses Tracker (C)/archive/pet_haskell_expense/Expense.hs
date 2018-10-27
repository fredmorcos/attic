module Expense where

import           Data.List
import           Extra.List
import           Extra.String
import           Extra.Tuple

type Command = Expenses -> IO Expenses

data Expense = Expense { amount :: Double
                       , person :: String
                       , shop   :: String
                       , date   :: (Int, Int, Int)
                       , tags   :: [String]
                       , note   :: String
                       } deriving Show

type Expenses = [Expense]

instance Eq Expense where
  (Expense a1 p1 s1 d1 t1 _) == (Expense a2 p2 s2 d2 t2 _) =
    a1 == a2 && p1 == p2 && s1 == s2 && d1 == d2 && sort t1 == sort t2

instance Ord Expense where
  compare (Expense a1 p1 s1 d1 _ _) (Expense a2 p2 s2 d2 _ _) =
    if null equals then EQ else head equals
    where equals = filter (/= EQ) [compare d1 d2, compare a1 a2,
                                   compare p1 p2, compare s1 s2]

displayExpenses :: Command
displayExpenses exps = mapM_ putStrLn table >> return exps
  where strExps = map (\e -> ( show $ amount e
                             , let d = date e
                                   prepad' = prepad '0' 2
                               in prepad' (show (fst3 d)) ++ "-"
                               ++ prepad' (show (snd3 d)) ++ "-"
                               ++ prepad' (show (trd3 d))
                             , person e
                             , shop e
                             , intercalate "," $ tags e
                             , note e
                             )) exps
        widths = foldl' (\(aa, ad, ap, as, at, an)
                          (ea, ed, ep, es, et, en) ->
                            ( max aa $ length ea
                            , max ad $ length ed
                            , max ap $ length ep
                            , max as $ length es
                            , max at $ length et
                            , max an $ length en))
                 (0, 0, 0, 0, 0, 0) strExps
        table = map (\(a, d, p, s, t, n) ->
                     let (la, ld, lp, ls, lt, ln) = widths
                         pad' = pad ' '
                     in pad' (la + 2) a ++ pad' (ld + 2) d ++
                        pad' (lp + 2) p ++ pad' (ls + 2) s ++
                        pad' (lt + 2) t ++ pad' (ln + 2) n) strExps

displayTotal :: Command
displayTotal exps = putStrLn ("Total amount: " ++ show total) >> return exps
  where total = sum $ map amount exps

selectPerson, selectTag, selectShop :: String -> Expenses -> IO Expenses
selectPerson v es = return $ filter (\e -> lower (person e) == lower v) es
selectTag    v es = return $ filter (\e -> lower v `elem` map lower (tags e)) es
selectShop   v es = return $ filter (\e -> lower (shop e) == lower v) es
