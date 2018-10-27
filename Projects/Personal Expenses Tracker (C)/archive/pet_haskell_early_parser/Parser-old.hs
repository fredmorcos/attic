module Parser where

data ParserState = Running { inputOf :: String
                           }
                 | Error   { stateOf :: ParserState
                           , msgOf   :: String
                           }
                 | End
                 deriving Show

type Predicate = (Char -> Bool)

-- parse zero or more (kleene star)
(|*) :: (ParserState, Int) -> Predicate -> (ParserState, Int)
fullState@(pState, len) |* p = case pState of
  Error {}           -> fullState
  End                -> fullState
  (Running [])       -> (End, len)
  (Running l@(i:is)) -> if p i then (Running is, len + 1) |* p
                        else (Running l, len)

-- parse one exactly
(|.) :: (ParserState, Int) -> Predicate -> (ParserState, Int)
fullState@(pState, len) |. p = case pState of
  Error {}         -> fullState
  End              -> fullState
  (Running [])     -> (Error pState "Premature end", len)
  (Running (i:is)) -> if p i then (Running is, len + 1)
                        else (Error pState "Could not match", len)

-- parse zero or one (at most one)
(|?) :: (ParserState, Int) -> Predicate -> (ParserState, Int)
fullState@(pState, len) |? p = case pState of
  Error {}         -> fullState
  End              -> fullState
  (Running [])     -> (End, len)
  (Running (i:is)) -> if p i then (Running is, len + 1)
                        else fullState

-- parse one or more (at least one), this is |. and then |*
(|+) :: (ParserState, Int) -> Predicate -> (ParserState, Int)
fullState@(_, _) |+ p = fullState |. p |* p
